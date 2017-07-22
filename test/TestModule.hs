module TestModule where

import qualified Data.Map as DM
import qualified Data.List as DL
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


withTopLevel :: String -> (TopLevel -> b) -> IO b
withTopLevel modname f = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname]) 
  return $ f topl

expected :: Show a => a -> a -> J ()
expected exp got = do nb $ "Expected : " ++ show exp
                      nb $ "Got      : " ++ show got
                      
hasTerminalsAt modname locs = do
  let qualModName = "/user/" ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  
  let func = "hasTerminalsAt" <? do
        topm <- TopLevel.getModule topl qualModName
        sub <- liftM head $ TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub        
        nb $ show sub
        m@(Module _ _ maybeIcon) <- TopLevel.getModule topl name
        bb <- case maybeIcon of
          (Just icon) -> do bb <- Icon.boundingBox icon
                            return bb
          _ -> die "Couldn't find icon."
        nb $ show bb
        terms <- Module.terminals m c3
        let locs' = [(x, y) | (Terminal (Coord3 x y _) _) <- terms]
        expected locs locs'
        return $ DL.sort locs == DL.sort locs'
  
  case runJ func of
    Right True -> putStrLn "PASS"
    Right False -> do
      putStrLn modname
      putStrLn $ runLog func
    Left msg -> do
      putStrLn modname
      putStrLn msg
      return undefined
  
testAll = do
  hasTerminalsAt "IconBoundingBox6" [(8,40),(24,40),(16,-8)]
  hasTerminalsAt "TermRot1" [(16,16), (-16,16), (-16,-16), (16, -16)]
  hasTerminalsAt "TermRot2" [(8,8),(40,8),(8,40),(40,40)]
  hasTerminalsAt "TermRot3" [(16,8),(16,40),(48,8),(48,40)]
  hasTerminalsAt "TermRot4" [(16,8),(16,40),(48,8),(48,40)]
  hasTerminalsAt "TermRot5" [(16,0),(16,32),(48,0),(48,32)]

  
  testSubModBoundingBox "SubBuiltIn1" $ BB {bbLeft=0, bbTop=(-4), bbRight=48, bbBottom=20}
  testSubModBoundingBox "SubBuiltIn2" $ BB {bbLeft=6, bbTop=(-16), bbRight=36, bbBottom=32}
  testSubModBoundingBox "SubBuiltIn3" $ BB {bbLeft=(-4), bbTop=(-48), bbRight=20, bbBottom=0}
  
  hasTerminalsAt "SubBuiltIn1" [(0,0), (0, 16), (48, 8)]
  hasTerminalsAt "SubBuiltIn2" [(16,32), (32, 32), (24, -16)]
  hasTerminalsAt "SubBuiltIn3" [(0,0), (16,0), (8,-48)]
  hasTerminalsAt "SubBuiltIn4" [(0,0), (16,0), (8,48)]
  
  --failing

testSubModBoundingBox modname exp = do 
  let qualModName = "/user/" ++ modname 
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = do
        sub <- liftM head $ TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub
        nb $ format "offset coord: {0}" [show c3]
        nb $ show sub
        m <- TopLevel.getModule topl name
        bb <- Module.boundingBox m c3
        if bb == exp
          then return ()
          else expected exp bb
          
  case runJ func of
    Right x -> do putStrLn "PASS"
                  return (Just x)
    Left msg -> do putStrLn msg
                   putStrLn $ runLog func
                   return Nothing



  






testBuiltInIconBoundingBox modname = do
  let qualModName = "/gates/" ++ modname 
  Right topl <- Decode.decodeTopLevel (format "./app-data/gates.json" [modname])

  let func = do
        m <- TopLevel.getModule topl qualModName
        sub <- liftM head $ TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub
        nb $ show c3
        m <- TopLevel.getModule topl name
        terms <- Module.terminals m c3
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

testRotateUseAND2Rot90 = do
  let modname = "UseAND2Rot90"
  let qualModName = "/user/" ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = do
        m <- TopLevel.getModule topl qualModName
        sub <- liftM head $ TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub
        nb $ show sub
        m <- TopLevel.getModule topl name
        terms <- Module.terminals m c3
        return terms
        --nb $ show $ map (\(Terminal c3 sig) -> (Coord.c3ToPoint c3, sig)) terms

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
        terms <- Module.terminals m c3
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
        terms <- Module.terminals m c3
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
        terms <- Module.terminals m c3
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
