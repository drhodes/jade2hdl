module Test.ModTest where

import qualified Jade.Decode as Decode
import qualified Jade.TopLevel as TopLevel
import qualified Jade.ModTest as ModTest
import Jade.Types

verify :: IO (Either String a) -> String -> IO ()
verify f testname = do
  x <- f
  case x of
    Left msg ->
      print $ "Fail: " ++ msg
    _ ->
      putStr "."

testLineSignals1 = do
  topl <- Decode.decodeTopLevel "./test-data/Tristate1.json"
  case topl of
    Right topl -> runJIO $ do
      Module schem mt _ <- TopLevel.getModule topl "/user/Tristate1"
      case mt of
        Just mt -> do let tlines = modTestLines mt
                      return $ putStrLn $ show tlines
    Left msg -> fail msg


