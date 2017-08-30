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
import TestUtil
import Jade.Rawr.Types 

withTopLevel :: String -> (TopLevel -> b) -> IO b
withTopLevel modname f = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname]) 
  return $ f topl

testSubModBoundingBox :: String -> BoundingBox -> IO TestState
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
    Right x -> return Pass
    Left msg -> return $ Fail $ unlines [msg, runLog func]


testBuiltInIconBoundingBox :: String -> IO (Maybe BoundingBox)
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

testRotateUseAND2Rot90 :: IO [Terminal]
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

  print $ [(16, -8), (8, 24), (24, 24)]
  case runJ func of
    Right x -> do print x
                  putStrLn $ runLog func
                  return x
    Left msg -> do putStrLn msg
                   return undefined

testIconBoundingBox5 :: IO (Maybe BoundingBox)
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

testIconBoundingBox5Rot90 :: IO (Maybe BoundingBox)
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

testIconBoundingBox6 :: IO (Maybe BoundingBox)
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

hasTerminalsAt2 :: String -> [(Integer, Integer)] -> IO TestState
hasTerminalsAt2 modname locs = do
  let qualModName = "/user/" ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  
  let func = "hasTerminalsAt2" <? do
        topm <- TopLevel.getModule topl qualModName
        sub <- head `liftM` TopLevel.getSubModules topl qualModName
        let (SubModule name c3) = sub        
        nb $ show sub
        m@(Module _ _ _ maybeIcon) <- TopLevel.getModule topl name
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
    Right True -> return Pass
    Right False -> return $ Fail $ unlines [ modname
                                           , runLog func ]
    Left msg -> return $ Fail $ unlines [ modname
                                        , msg ]


t name iofunc exp = TestNode $ Case name (iofunc name exp)

testTreeHasTerminalsAt = TestTree "hasTerminalsAt" $
  [ t "IconBoundingBox6" hasTerminalsAt2 [(8,40),(24,40),(16,-8)]
  , t "TermRot1" hasTerminalsAt2 [(16,16), (-16,16), (-16,-16), (16, -16)]
  , t "TermRot2" hasTerminalsAt2 [(8,8),(40,8),(8,40),(40,40)]
  , t "TermRot3" hasTerminalsAt2 [(16,8),(16,40),(48,8),(48,40)]
  , t "TermRot4" hasTerminalsAt2 [(16,8),(16,40),(48,8),(48,40)]
  , t "TermRot5" hasTerminalsAt2 [(16,0),(16,32),(48,0),(48,32)]
  , t "SubBuiltIn1" hasTerminalsAt2 [(0,0), (0, 16), (48, 8)]
  , t "SubBuiltIn1" hasTerminalsAt2 [(0,0), (0, 16), (48, 8)]
  , t "SubBuiltIn2" hasTerminalsAt2 [(16,32), (32, 32), (24, -16)]
  , t "SubBuiltIn3" hasTerminalsAt2 [(0,0), (16,0), (8,-48)]
  , t "SubBuiltIn4" hasTerminalsAt2 [(0,0), (16,0), (8,48)]
  ]

testTreeSubModBoundingBox = TestTree "subModBoundingBox" $
  [ t "SubBuiltIn3" testSubModBoundingBox BB{bbLeft=(-4), bbTop=(-48), bbRight=20, bbBottom=0}
  , t "SubBuiltIn1" testSubModBoundingBox BB{bbLeft=0, bbTop=(-4), bbRight=48, bbBottom=20}
  , t "SubBuiltIn2" testSubModBoundingBox BB{bbLeft=6, bbTop=(-16), bbRight=36, bbBottom=32}
  ]


testTree = TestTree "TestModule" [ testTreeSubModBoundingBox
                                 , testTreeHasTerminalsAt
                                 ]
 
