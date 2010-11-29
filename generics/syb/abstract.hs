{-# LANGUAGE DeriveDataTypeable #-}
import Data.Generics

data Expr = Num Int
          | Val String
          | Let Decl Expr
          | Plus  Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div   Expr Expr
            deriving (Show, Eq, Data, Typeable)

data Decl = String := Expr
          | Seq Decl Decl
            deriving (Show, Eq, Data, Typeable)

normalize' (Plus (Num n) (Num m))  = Num (n + m)
normalize' (Multi (Num n) (Num m)) = Num (n * m)
normalize' (Minus (Num n) (Num m)) = Num(n - m)
normalize' (Div (Num n) (Num m))   = Num(n `div` m)
normalize' x                       = x

-- | Can be used for any type including Expr
normalize :: Data a => a -> a
normalize = everywhere (mkT normalize')

-- 2 * (3 + 2 * 5) == 26
expr1 = Multi (Num 2) (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- a * (3 + 2 * 5) == a * 13
expr2 = Multi (Val "a") (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- let a := 2 * 5 in 3 + 2 == let a := 10 in 5
expr3 = Let ("a" := Multi (Num 2) (Num 5)) (Plus (Num 3) (Num 2))

-- let a := 3 * 4 == let a := 12
decl1 = "a" := Multi (Num 3) (Num 4)


badNormalize :: Expr -> Expr
badNormalize = everywhere' (mkT normalize')