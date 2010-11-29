{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import System.Random

$( do rnd <- runIO $ randomRIO (0,1)
      let nm = mkName (["a", "b"] !! rnd)
      m <- [d| main = $(varE $ mkName "a") |]
      t <- valD (varP nm) (normalB [| putStrLn "ヽ( ´ー`)ノ" |]) []
      return (t:m)
  )
