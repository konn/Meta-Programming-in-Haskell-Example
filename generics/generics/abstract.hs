{-# LANGUAGE Generics, TypeOperators, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances, TypeSynonymInstances #-}
import Data.Generics

data Expr = Num Int
          | Val String
          | Let Decl Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div   Expr Expr
            deriving (Show, Eq)

data Decl = String := Expr
          | Seq Decl Decl
            deriving (Show, Eq)

class Normalize a where
    normalize :: a -> a
    
    normalize {|  Unit   |} Unit      = Unit
    normalize {| a :*: b |} (a :*: b) = normalize a :*: normalize b
    normalize {| a :+: b |} (Inl a)   = Inl (normalize a)
    normalize {| a :+: b |} (Inr b)   = Inr (normalize b)

instance Normalize Int where
  normalize = id
instance Normalize String where normalize = id

instance Normalize a => Normalize [a]
instance Normalize a => Normalize (Maybe a)
instance Normalize Bool

instance Normalize Expr where
  normalize (Plus n m)  = case (normalize n, normalize m) of
                            (Num n, Num m) -> Num (n + m)
                            (n, m)         -> Plus n m
  normalize (Minus n m) = case (normalize n, normalize m) of
                            (Num n, Num m) -> Num (n - m)
                            (n, m)         -> Minus n m
  normalize (Multi n m) = case (normalize n, normalize m) of
                            (Num n, Num m) -> Num (n * m)
                            (n, m)         -> Multi n m
  normalize (Div n m)   = case (normalize n, normalize m) of
                            (Num n, Num m) -> Num (n `div` m)
                            (n, m)         -> Div n m
  normalize (Let d e)   = Let (normalize d) (normalize e)
  normalize e           = e

instance Normalize Decl

-- 2 * (3 + 2 * 5) == 26
expr1 = Multi (Num 2) (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- a * (3 + 2 * 5) == a * 13
expr2 = Multi (Val "a") (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- let a := 2 * 5 in 3 + 2 == let a := 10 in 5
expr3 = Let ("a" := Multi (Num 2) (Num 5)) (Plus (Num 3) (Num 2))

-- let a := 3 * 4 == let a := 12
decl1 = "a" := Multi (Num 3) (Num 4)
