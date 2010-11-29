{-# LANGUAGE Generics, TypeOperators, TypeFamilies, FlexibleContexts #-}
import Data.Generics

data Expr = Num Int
          | Val String
          | Plus Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div   Expr Expr
            deriving (Show, Eq)

class Normalize a where
    normalize :: a -> a
    
    normalize {|  Unit   |} Unit      = Unit
    normalize {| a :*: b |} (a :*: b) = normalize a :*: normalize b
    normalize {| a :+: b |} (Inl a)   = Inl (normalize a)
    normalize {| a :+: b |} (Inr b)   = Inr (normalize b)

instance Normalize Int where
  normalize = id

instance Normalize a => Normalize [a]
instance Normalize a => Normalize (Maybe a)
instance Normalize Bool

{-
instance Normalize Expr where
  normalize (Plus (Num n) (Num m))  = Num (n + m)
  normalize (Multi (Num n) (Num m)) = Num (n * m)
  normalize (Minus (Num n) (Num m)) = Num (n - m)
  normalize (Div (Num n) (Num m))   = Num (n `div` m)
  normalize (Plus (Var a) 
-}

-- 2 * (3 + 2 * 5) = 26
tree1 = Multi (Num 2) (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- a * (3 + 2 * 5) = a * 13
tree2 = Multi (Val "a") (Plus (Num 3) (Multi (Num 2) (Num 5)))

