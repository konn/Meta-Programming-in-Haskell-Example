{-# LANGUAGE DeriveDataTypeable #-}
import Data.Generics

data Expr = Num Int
          | Var String
          | Plus Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div   Expr Expr
            deriving (Show, Eq, Data, Typeable)

normalize' (Plus (Num n) (Num m))  = Num (n + m)
normalize' (Multi (Num n) (Num m)) = Num (n * m)
normalize' (Minus (Num n) (Num m)) = Num(n - m)
normalize' (Div (Num n) (Num m))   = Num(n `div` m)
normalize' x                       = x

normalize :: Expr -> Expr
normalize = everywhere (mkT normalize')

-- 2 * (3 + 2 * 5) = 26
tree1 = Multi (Num 2) (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- a * (3 + 2 * 5) = a * 13
tree2 = Multi (Var "a") (Plus (Num 3) (Multi (Num 2) (Num 5)))

badNormalize :: Expr -> Expr
badNormalize = everywhere' (mkT normalize')