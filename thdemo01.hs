{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Cocoa hiding (main)
import Control.Concurrent

-- For Mac OS X Only (HOC is required)
-- Play the "Symphony No. 5" by Beethoven
$(
  do runIO $ withAutoreleasePool $ do 
       s <- _NSSound # alloc >>= initWithContentsOfFileByReference (toNSString "/path/to/Music/File/") False
       (castObject s :: NSSound ()) # play
       threadDelay (7*60*10^6)
     [d| main = putStrLn "hoge" |]
 )