module TestModule where

import qualified Data.Map as DM
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Icon as Icon
import qualified Jade.Coord as Coord
import qualified Jade.Vhdl as Vhdl
import qualified Data.Hashable as H
import Text.Format
import Control.Monad

testRotateUseAND2Rot90 = do
  let modname = "UseAND2Rot90"
  --let modname = "UseAND2"
  let qualModName = "/user/" ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])


  let func = do
        m <- TopLevel.getModule topl qualModName
        sub <- liftM head $ TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub
        nb $ show c3
        m <- TopLevel.getModule topl name
        terms <- Module.testTerms m c3
        nb $ show $ map (\(Terminal c3 sig) -> (Coord.c3ToPoint c3, sig)) terms

        let Just icon = moduleIcon m
        Icon.boundingBox icon

  print $ [(16, -8), (8, 24), (24, 24)]
  case runJ func of
    Right x -> do print x
                  putStrLn $ runLog func
                  return x
    Left msg -> do putStrLn msg
                   return undefined



testIconBoundingBox5 = do
  let modname = "IconBoundingBox5"
  let qualModName = "/user/" ++ modname 
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = do
        m <- TopLevel.getModule topl qualModName
        sub <- liftM head $ TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub
        nb $ show c3
        m <- TopLevel.getModule topl name
        terms <- Module.testTerms m c3
        nb $ show $ map (\(Terminal c3 sig) -> (Coord.c3ToPoint c3, sig)) terms
        let Just icon = moduleIcon m
        bb <- Icon.boundingBox icon
        return bb
  
  print $ [(16, -8), (8, 24), (24, 24)]
  case runJ func of
    Right x -> do print x
                  putStrLn $ runLog func
                  return (Just x)
    Left msg -> do putStrLn msg
                   putStrLn $ runLog func
                   return Nothing


testIconBoundingBox5Rot90 = do
  let modname = "IconBoundingBox5Rot90"
  let qualModName = "/user/" ++ modname 
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = do
        m <- TopLevel.getModule topl qualModName
        sub <- liftM head $ TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub
        nb $ show c3
        m <- TopLevel.getModule topl name
        terms <- Module.testTerms m c3
        nb $ show $ map (\(Terminal c3 sig) -> (c3, sig)) terms
        nb $ show $ map (\(Terminal c3 sig) -> (Coord.c3ToPoint c3, sig)) terms
        let Just icon = moduleIcon m
        bb <- Icon.boundingBox icon
        return bb
  
  case runJ func of
    Right x -> do print x
                  putStrLn $ runLog func
                  return (Just x)
    Left msg -> do putStrLn msg
                   putStrLn $ runLog func
                   return Nothing

testIconBoundingBox6 = do
  let modname = "IconBoundingBox6"
  let qualModName = "/user/" ++ modname 
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = do
        m <- TopLevel.getModule topl qualModName
        sub <- liftM head $ TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub
        nb $ show c3
        m <- TopLevel.getModule topl name
        terms <- Module.testTerms m c3
        --nb $ show $ map (\(Terminal c3 sig) -> (Coord.c3ToPoint , sig)) terms
        nb $ show $ map (\(Terminal c3 sig) -> (c3, sig)) terms
        let Just icon = moduleIcon m
        bb <- Icon.boundingBox icon
        return bb
  
  case runJ func of
    Right x -> do print x
                  putStrLn $ runLog func
                  return (Just x)
    Left msg -> do putStrLn msg
                   putStrLn $ runLog func
                   return Nothing







-- testEntityDecl = do
--   Right (TopLevel m) <- Decode.decodeTopLevel "./test-data/and2.json"
--   let pair = head $ DM.toList m
--   print $ P.pp $ Vhdl.mkEntityDecl pair

-- testMkArchBody = do
--   Right (TopLevel m) <- Decode.decodeTopLevel "./test-data/and2.json"
--   let pair = head $ DM.toList m
--   print $ P.pp $ Vhdl.mkArchBody pair










{-
testGraphVoodoo filename modname = do
  Right tl <- Decode.decodeTopLevel filename  
  case runJ $ TopLevel.getModule tl modname of
    Right mod -> do
      let comps = TopLevel.components mod
      print comps
      print $ (filename, modname, "Numcomponents", length comps)
    Nothing -> print $ "couldn't find module: " ++ modname ++ " in toplevel"

testGraphAnd2 = testGraphVoodoo "./test-data/and2-with-wires.json" "/user/AND2"
testGraphPortWire = testGraphVoodoo "./test-data/port-wire.json" "/user/port_wire"
testGraphPortWireMovedUp =
  testGraphVoodoo "./test-data/port-wire-moved-up.json" "/user/port_wire"

testGraphSubModule filename modname = do
  Right tl <- Decode.decodeTopLevel filename
  case TopLevel.getModule tl modname of
    Just (Module schem _) -> print $ schem
    Nothing -> print $ "couldn't find module: " ++ modname ++ " in toplevel"

testGraphSubModuleAnd2 =
  testGraphSubModule "./test-data/and2-with-wires.json" "/user/AND2"
-}

-- testConnected = do
--   Right (TopLevel m) <- D.decodeTopLevel "./test-data/fan5rot4connect.json"
--   case DM.elems m of
--     [Module (Just (Schematic wirecs)) _ _] -> 
--       let wires = [w | WireC w <- DV.toList wirecs]
--           edges = map wireToEdge wires
--           g = G.fromEdges edges
--       in return g
--     x -> fail "No schematic found in Module.testConnected"
