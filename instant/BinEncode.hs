{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, EmptyDataDecls #-}
import Generics.Instant
import Data.List

class Bin a where
    toBin :: a -> [Int]
    fromBin :: [Int] -> (a, [Int])

instance Bin U where
    toBin   U  = []
    fromBin xs = (U, [])

instance (Bin a, Bin b) => Bin (a :+: b) where
    toBin (L a) = 0:toBin a
    toBin (R b) = 1:toBin b
    fromBin (0:bin) = let (a, bin') = fromBin bin in (L a, bin')
    fromBin (1:bin) = let (b, bin') = fromBin bin in (R b, bin')

instance (Bin a, Bin b) => Bin (a :*: b) where
    toBin (a :*: b) = toBin a ++ toBin b
    fromBin bin     =
      let (a, bin')  = fromBin bin
          (b, bin'') = fromBin bin'
      in (a :*: b, bin'')

instance Bin a => Bin (Rec a) where
    toBin (Rec r) = toBin r
    fromBin bin   = let (r, bin') = fromBin bin in (Rec r, bin')

instance Bin a => Bin (C c a) where
    toBin (C a)  = toBin a
    fromBin bin  = let (a, bin') = fromBin bin in (C a, bin')

instance Bin a => Bin (Var a) where
    toBin (Var a) = toBin a
    fromBin bin   = let (x, bin') = fromBin bin in (Var x, bin')

def_toBin :: (Representable a, Bin (Rep a)) => a -> [Int]
def_toBin = toBin . from

def_fromBin :: (Representable a, Bin (Rep a)) => [Int] -> (a, [Int])
def_fromBin bin = let (rep, bin') = fromBin bin in (to rep, bin')


instance Bin Int where
    toBin i   = let sign   = if i < 0 then 1 else 0
                    i'     = abs i
                    binary = toBits i'
                in take (bitCount + 1) (sign:binary ++ replicate bitCount 0)
    fromBin a = let (s:as, bs) = splitAt (1+bitCount) a
                    sign = [1, -1] !! s
                in (sign*fromBits as, bs)

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

instance Bin Char where
  toBin      = take 21 . (++replicate 21 0) . toBits . fromEnum
  fromBin bs = let (ch, bs') = splitAt 21 bs in (toEnum $ fromBits ch, bs')

instance Bin a => Bin [a] where { toBin = def_toBin; fromBin = def_fromBin }
instance Bin Bool where { toBin = def_toBin; fromBin = def_fromBin }
instance Bin a => Bin (Maybe a) where { toBin = def_toBin; fromBin = def_fromBin }
instance (Bin a, Bin b) => Bin (Either a b) where { toBin = def_toBin; fromBin = def_fromBin }
instance (Bin a, Bin b) => Bin (a, b) where { toBin = def_toBin; fromBin = def_fromBin }

unfoldrStep :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> Maybe (a, b)
unfoldrStep p f g x | p x       = Nothing
                    | otherwise = Just (f x, g x)

bitCount = ceiling $ logBase 2 $ fromIntegral (maxBound::Int)
toBits   = unfoldr (unfoldrStep (==0) (`mod`2) (`div`2))
fromBits = foldr (\a b -> a+2*b) 0