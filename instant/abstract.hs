{-# LANGUAGE TypeOperators, EmptyDataDecls, TypeFamilies, FlexibleContexts #-}
import Generics.Instant

-- 直に木を舐めるだけの実装
-- everywhere の用に任意の構造体中の木を舐めたければ
-- 各所のコメントを参考にに実装してください
data Expr = Num Int
          | Val String
          | Plus Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div   Expr Expr
            deriving (Show, Eq)

-- 構築子に対応するダミーの型名
-- 構築子の情報を保持するのに必要
data Expr_Num; data Expr_Val; data Expr_Plus
data Expr_Minus; data Expr_Multi; data Expr_Div

-- 構築子であることを表わす型クラス
-- 演算子型の構築子の場合は結合性の情報も指定出来る
-- conName :: Constructor con ⇒ t c a → String
-- hoge c@(C a) = conName c  などとすれば，構築子名が取れる
instance Constructor Expr_Num   where conName _ = "Num"
instance Constructor Expr_Val   where conName _ = "Val"
instance Constructor Expr_Plus  where conName _ = "Plus"
instance Constructor Expr_Minus where conName _ = "Minus"
instance Constructor Expr_Multi where conName _ = "Multi"
instance Constructor Expr_Div   where conName _ = "Div"

instance Representable Expr where
    type Rep Expr = C Expr_Num (Var Int) 
                :+: C Expr_Val (Var String) 
                :+: C Expr_Plus (Rec Expr :*: Rec Expr)
                :+: C Expr_Minus (Rec Expr :*: Rec Expr)
                :+: C Expr_Multi (Rec Expr :*: Rec Expr)
                :+: C Expr_Div (Rec Expr :*: Rec Expr)
    from (Num i)     = L (C $ Var i)
    from (Val s)     = R (L (C $ Var s))
    from (Plus i j)  = R (R (L (C $ Rec i :*: Rec j)))
    from (Minus i j) = R (R (R (L (C $ Rec i :*: Rec j))))
    from (Multi i j) = R (R (R (R (L (C $ Rec i :*: Rec j)))))
    from (Div i j)   = R (R (R (R (R (C $ Rec i :*: Rec j)))))

    to (L (C (Var i)))                           = Num i
    to (R (L (C (Var s))))                       = Val s
    to (R (R (L (C (Rec i :*: Rec j)))))         = Plus i j
    to (R (R (R (L (C (Rec i :*: Rec j))))))     = Minus i j
    to (R (R (R (R (L (C (Rec i :*: Rec j))))))) = Multi i j
    to (R (R (R (R (R (C (Rec i :*: Rec j))))))) = Div i j

class Normalize a where
    normalize :: a -> a
    normalize = id

dft_normalize :: (Representable a, Normalize (Rep a)) => a -> a
dft_normalize = to . normalize . from

instance Normalize U

-- 任意の構造中の木を舐める様にするには，Var の中も再帰的に舐める必要がある．
-- 今回は構文木木だけを考えているので，Varの中身は舐めない．
instance Normalize (Var a)

instance (Normalize a, Normalize b) => Normalize (a :+: b) where
    normalize (L a) = L (normalize a)
    normalize (R b) = R (normalize b)

instance (Normalize a, Normalize b) => Normalize (a :*: b) where
    normalize (a :*: b) = normalize a :*: normalize b

instance Normalize a => Normalize (Rec a) where
    normalize (Rec a) = Rec (normalize a)

instance Normalize a => Normalize (C con a) where
    normalize (C a) = C (normalize a)

{- Varの中も舐めるのなら，次の宣言が必要
-- normalize のデフォルト定義が id なので，instance と書くだけでいい
-- instance Normalize Int
-- instance Normalize Char
-- 他のデータ中の木を舐めたければ，次の宣言も追加
-- instance Normalzie a => Normalize (Maybe a) where normalize = dft_normalize
-- instance Normalzie a => Normalize [a] where normalize = dft_normalize
-- etc, etc...
-}

instance Normalize Expr where
  normalize x = case dft_normalize x of
                  Plus (Num n) (Num m)  -> Num (n + m)
                  Multi (Num n) (Num m) -> Num (n * m)
                  Minus (Num n) (Num m) -> Num(n - m)
                  Div (Num n) (Num m)   -> Num(n `div` m)
                  x                     -> x

-- 2 * (3 + 2 * 5) = 26
tree1 = Multi (Num 2) (Plus (Num 3) (Multi (Num 2) (Num 5)))

-- a * (3 + 2 * 5) = a * 13
tree2 = Multi (Val "a") (Plus (Num 3) (Multi (Num 2) (Num 5)))

