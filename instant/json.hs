{-# LANGUAGE TypeOperators, EmptyDataDecls, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE  TypeSynonymInstances, ScopedTypeVariables, OverlappingInstances #-}
import Generics.Instant
import Control.Applicative


-- ある程度見やすいJSONシリアライザ
-- コンストラクタの名前が保存される
-- ただしあくまでもある程度みやすい程度
data JSValue = JSNumber Rational
             | JSString String
             | JSNull
             | JSArray  [JSValue]
             | JSObject [(String, JSValue)]
               deriving (Show, Eq)

class JSON a where
    toJSON   :: a -> JSValue
    fromJSON :: JSValue -> Maybe a

instance JSON JSValue where
    toJSON   = id
    fromJSON = Just

instance JSON Int where
    toJSON = JSNumber . fromIntegral
    fromJSON (JSNumber j) = Just $ floor j
    fromJSON _ = Nothing

instance JSON String where
    toJSON = JSString
    fromJSON (JSString s) = Just s
    fromJSON _ = Nothing

instance JSON Char where
    toJSON = JSString . return
    fromJSON (JSString [x]) = Just x
    fromJSON _              = Nothing

instance JSON a => JSON (Var a) where
    toJSON (Var a) = toJSON a
    fromJSON     x = Var <$> fromJSON x

instance (JSON a, JSON b) => JSON (a :+: b) where
    toJSON (L a) = JSObject [("inL", toJSON a)]
    toJSON (R b) = JSObject [("inR", toJSON b)]
    fromJSON (JSObject dic) = L <$> (fromJSON =<< lookup "inL" dic)
                          <|> R <$> (fromJSON =<< lookup "inR" dic)

instance (JSON a, JSON b) => JSON (a :*: b) where
    toJSON (a :*: b) = JSArray [toJSON a, toJSON b]
    fromJSON (JSArray [x, y]) = (:*:) <$> fromJSON x <*> fromJSON y

instance JSON a => JSON (Rec a) where
    toJSON   (Rec a) = toJSON a
    fromJSON j       = Rec <$> fromJSON j

instance JSON U where
    toJSON = const JSNull
    fromJSON JSNull = Just U
    fromJSON _      = Nothing

instance (JSON a, Constructor con) => JSON (C con a) where
    toJSON con@(C a) = JSObject [(conName con, toJSON a)] -- 構築子名を得ている
    fromJSON (JSObject [(con, bs)]) = C <$> fromJSON bs
    fromJSON _                      = Nothing

instance (JSON a, JSON b) => JSON (a, b) where toJSON = dft_toJSON; fromJSON = dft_fromJSON
instance (JSON a, JSON b, JSON c) => JSON (a, b, c) where toJSON = dft_toJSON; fromJSON = dft_fromJSON
instance (JSON a, JSON b) => JSON (Either a b) where toJSON = dft_toJSON; fromJSON = dft_fromJSON
instance JSON a => JSON [a] where toJSON = dft_toJSON; fromJSON = dft_fromJSON
instance JSON a => JSON (Maybe a) where toJSON = dft_toJSON; fromJSON = dft_fromJSON

data Triple_C_
instance Constructor Triple_C_ where conName _ = "(,,)"

instance Representable (a, b, c) where
    type Rep (a, b, c) = C Triple_C_ (Var a :*: Var b :*: Var c)
    from (a, b, c) = C (Var a :*: Var b :*: Var c)
    to (C (Var a :*: Var b :*: Var c)) = (a,b,c)

dft_toJSON :: (JSON (Rep a), Representable a) => a -> JSValue
dft_toJSON = toJSON . from
dft_fromJSON :: (JSON (Rep a), Representable a) => JSValue -> Maybe a
dft_fromJSON js = to <$> fromJSON js


data Either_Left
data Either_Right

instance Constructor Either_Left where conName _ = "Left"
instance Constructor Either_Right where conName _ = "Right"

instance Representable (Either a b) where
    type Rep (Either a b) = C Either_Left (Var a) :+: C Either_Right (Var b)
    from (Left a)  = L (C (Var a))
    from (Right b) = R (C (Var b))
    to (L (C (Var a))) = Left a
    to (R (C (Var b))) = Right b
