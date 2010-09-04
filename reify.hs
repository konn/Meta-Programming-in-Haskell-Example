{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH

$(
  do listInfo  <- reify ''[]
     printInfo <- reify 'print
     runIO $ do
       putStrLn "Information of List Type: "
       print listInfo >> putStrLn ""
       putStrLn $ pprint listInfo
       putStrLn "\n------------------"
       putStrLn "Information of print function: " 
       print printInfo >> putStrLn ""
       putStrLn $ pprint printInfo

     [d| main = putStrLn "It's just a testing!" |]
 )