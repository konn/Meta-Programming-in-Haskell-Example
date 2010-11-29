{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where
import Data.Generics.Uniplate.Direct
import Data.Generics

data Expr = Num Int
          | Val String
          | Let   Decl Expr
          | Plus  Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div   Expr Expr
            deriving (Show, Eq)

data Decl = String := Expr
          | Seq Decl Decl
            deriving (Show, Eq) 

instance Uniplate Expr where
  uniplate (Num i)       = plate Num   |- i
  uniplate (Val v)       = plate Val   |- v
  uniplate (Let d e)     = plate Let   |+ d  |* e
  uniplate (Plus  e1 e2) = plate Plus  |* e1 |* e2
  uniplate (Minus e1 e2) = plate Minus |* e1 |* e2
  uniplate (Multi e1 e2) = plate Multi |* e1 |* e2
  uniplate (Div   e1 e2) = plate Div   |* e1 |* e2

instance Biplate Expr Expr where
  biplate = plateSelf

instance Uniplate Decl where
  uniplate (v := expr) = plate (:=) |- v  |+ expr
  uniplate (Seq d1 d2) = plate Seq  |* d1 |* d2

instance Biplate Decl Decl where
  biplate = plateSelf

instance Biplate Decl Expr where
  biplate (v := expr) = plate (:=) |- v  |* expr
  biplate (Seq d1 d2) = plate Seq  |+ d1 |+ d2

instance Biplate Expr Decl where
  biplate (Num i)       = plate Num   |- i
  biplate (Val v)       = plate Val   |- v
  biplate (Let d e)     = plate Let   |* d  |+ e
  biplate (Plus  e1 e2) = plate Plus  |+ e1 |+ e2
  biplate (Minus e1 e2) = plate Minus |+ e1 |+ e2
  biplate (Multi e1 e2) = plate Multi |+ e1 |+ e2
  biplate (Div   e1 e2) = plate Div   |+ e1 |+ e2


tr' :: Expr -> Expr
tr' (Plus (Num i) (Num j)) = Num (i + j)
tr' (Multi (Num i) (Num j)) = Num (i * j)
tr' x                      = x

normalizeE :: Expr -> Expr 
normalizeE = transform tr'

normalizeD :: Decl -> Decl 
normalizeD = transformBi tr'

normalize2 = rewrite f
  where
    f (Plus  (Num i) (Num j)) = Just $ Num (i + j)
    f (Multi (Num i) (Num j)) = Just $ Num (i * j)
    f _                       = Nothing

-- 2 * (3 + 2 * 5) == 26
expr1 = Multi (Num 2) (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- a * (3 + 2 * 5) == a * 13
expr2 = Multi (Val "a") (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- let a := 2 * 5 in 3 + 2 == let a := 10 in 5
expr3 = Let ("a" := Multi (Num 2) (Num 5)) (Plus (Num 3) (Num 2))

-- let a := 3 * 4 == let a := 12
decl1 = "a" := Multi (Num 3) (Num 4)
