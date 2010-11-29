import Data.Generics.Multiplate
import Control.Applicative
import Data.Functor.Identity

data Expr = Num Int
          | Val String
          | Let Decl Expr
          | Plus  Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div   Expr Expr
            deriving (Show, Eq)

data Decl = String := Expr
          | Seq Decl Decl
            deriving (Show, Eq)

data Plate f = Plate { expr :: Expr -> f Expr
                     , decl :: Decl -> f Decl}

instance Multiplate Plate where
  multiplate plate = Plate buildExpr buildDecl
    where
      buildExpr (Plus  e1 e2) = Plus  <$> expr plate e1 <*> expr plate e2
      buildExpr (Minus e1 e2) = Minus <$> expr plate e1 <*> expr plate e2
      buildExpr (Multi e1 e2) = Multi <$> expr plate e1 <*> expr plate e2
      buildExpr (Div   e1 e2) = Div   <$> expr plate e1 <*> expr plate e2
      buildExpr e             = pure e
      buildDecl (v := e)    = (:=) <$> pure v <*> expr plate e
      buildDecl (Seq d1 d2) = Seq  <$> decl plate d1 <*> decl plate d2
  mkPlate build = Plate (build expr) (build decl)

normalizer :: Plate Identity
normalizer = mapFamily myPlate
  where
    myPlate = (purePlate :: Plate Identity) {expr = subE, decl = subD}
    subE (Plus  (Num i) (Num j)) = pure $ Num (i + j)
    subE (Minus (Num i) (Num j)) = pure $ Num (i - j)
    subE (Multi (Num i) (Num j)) = pure $ Num (i * j)
    subE (Div   (Num i) (Num j)) = pure $ Num (i `div` j)
    subE (Let d1 e1)             = Let <$> subD d1 <*> subE e1
    subE e                       = pure e
    subD (v := e)                = (v :=) <$> subE e
    subD d                       = pure d

normalizeE :: Expr -> Expr
normalizeE = traverseFor expr normalizer

normalizeD :: Decl -> Decl
normalizeD = traverseFor decl normalizer

-- 2 * (3 + 2 * 5) == 26
expr1 = Multi (Num 2) (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- a * (3 + 2 * 5) == a * 13
expr2 = Multi (Val "a") (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- let a := 2 * 5 in 3 + 2 == let a := 10 in 5
expr3 = Let ("a" := Multi (Num 2) (Num 5)) (Plus (Num 3) (Num 2))

-- let a := 3 * 4 == let a := 12
decl1 = "a" := Multi (Num 3) (Num 4)
