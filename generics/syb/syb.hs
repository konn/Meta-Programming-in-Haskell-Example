{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, StandaloneDeriving, GADTs, FlexibleInstances #-}
import Data.Generics
import Data.GraphViz
import Data.DeriveTH

data Tree = Leaf Int | Branch Tree Tree 
            deriving (Show, Eq, Ord, Data, Typeable)

isLeaf (Leaf _) = True
isLeaf _        = False