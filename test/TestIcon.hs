module TestIcon where

import qualified Data.Map as DM
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Icon as Icon
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Coord as Coord
import qualified Jade.Vhdl as Vhdl
import qualified Data.Hashable as H
import Text.Format
import Control.Monad


expected :: Show a => a -> a -> J ()
expected exp got = die $ format "Expected {0}, Got: {1}" [show exp, show got]

testBoundingBox = do
  let modname = "IconBoundingBox1"
  let qualModName = "/user/" ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = "testBoundingBox" <? do
        m <- TopLevel.getModule topl qualModName
        case moduleIcon m of
          Nothing -> die $ "No icon found for module: " ++ qualModName
          Just icon -> do
            (topLeft, botRight) <- Icon.boundingBox icon
            let expectedTopLeft = (-16, -16)
                expectedBottomRight = (16, 16)
            "topLeft" <? (when (expectedTopLeft /= topLeft) $ expected expectedTopLeft topLeft)
            "botRight" <? (when (expectedBottomRight /= botRight) $ expected expectedBottomRight botRight)
            
  case runJ func of
    Right x -> do print x
                  putStrLn $ runLog func
                  return x
    Left msg -> do putStrLn msg
                   putStrLn $ runLog func
                   return undefined
