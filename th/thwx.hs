{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -framework Carbon #-}
import Graphics.UI.WX
import EnableGUI
import Language.Haskell.TH

runIO $ do
  start $ do
    f <- frameFixed [text := "はろーWorld!"]
    p <- panel f []
    set f [layout := minsize (sz 300 300) $ widget p]
  return []