{-# LANGUAGE TemplateHaskell, Generics, TypeOperators, DeriveDataTypeable, RankNTypes, GADTs #-}
module Main where
import Data.Generics
import Data.List
import Data.Map

type BinS a = (a, [Int])
class Bin a where
    toBin   :: a -> [Int]
    fromBin :: [Int] -> (a, [Int])
                
    toBin {|  Unit   |} Unit      = []
    toBin {| a :+: b |} (Inl x)   = 0:toBin x
    toBin {| a :+: b |} (Inr y)   = 1:toBin y
    toBin {| a :*: b |} (x :*: y) = toBin x ++ toBin y
    
    fromBin {| Unit  |} bs     = (Unit, bs)
    fromBin {|a :+: b|} (0:bs) = (Inl x, bs') where (x, bs') = fromBin bs
    fromBin {|a :+: b|} (1:bs) = (Inr y, bs') where (y, bs') = fromBin bs
    fromBin {|a :*: b|} bs     = (x :*: y, bs'')
        where
          (x, bs')  = fromBin bs
          (y, bs'') = fromBin bs'

instance Bin Int where
    toBin i   = let sign   = if i < 0 then 1 else 0
                    i'     = abs i
                    binary = toBits i'
                in take (bitCount + 1) (sign:binary ++ replicate bitCount 0)
    fromBin a = let (s:as, bs) = splitAt (1+bitCount) a
                    sign = [1, -1] !! s
                in (sign*fromBits as, bs)

instance Bin Char where
  toBin      = take 21 . (++replicate 21 0) . toBits . fromEnum
  fromBin bs = let (ch, bs') = splitAt 21 bs in (toEnum $ fromBits ch, bs')

instance Bin ()
instance Bin Bool
instance (Bin a) => Bin (Maybe a)
instance (Bin a, Bin b) => Bin (Either a b)
instance Bin Ordering

instance (Bin a, Bin b) => Bin (a, b)
instance (Bin a, Bin b, Bin c) => Bin (a, b, c)
instance Bin a => Bin [a]
instance (Bin k, Bin v) => Bin (Map k v)



unfoldrStep :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> Maybe (a, b)
unfoldrStep p f g x | p x       = Nothing
                    | otherwise = Just (f x, g x)

bitCount = ceiling $ logBase 2 $ fromIntegral (maxBound::Int)
toBits   = unfoldr (unfoldrStep (==0) (`mod`2) (`div`2))
fromBits = foldr (\a b -> a+2*b) 0