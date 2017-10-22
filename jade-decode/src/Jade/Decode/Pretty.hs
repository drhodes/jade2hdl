module Jade.Decode.Pretty where

import Text.PrettyPrint.Leijen hiding (Mode)
import Jade.Decode.Types
import Control.Monad
import Data.Maybe
import qualified Data.Map as DM

instance Pretty Line where
  pretty (Line c5) = text "Line: " <+> pretty c5

instance Pretty Terminal where
  pretty (Terminal c s) = braces $ text "Terminal" <+> pretty c <+> pretty s

instance Pretty Box where
  pretty (Box c) = braces $ text "Box" <+> pretty c

instance Pretty Txt where
  pretty (Txt loc txt _) = braces $ text "Txt" <+> pretty loc <+> text txt

instance Pretty Circle where
  pretty (Circle x y r) = braces $ text "Circle" <+> pretty (x, y) <+> text "radius:" <+> pretty r

instance Pretty IconPart where
  pretty (IconLine x) = pretty x
  pretty (IconTerm x) = pretty x
  pretty (IconBox x) = pretty x
  pretty (IconTxt x) = pretty x
  pretty (IconCircle x) = pretty x
  pretty x = braces $ text $ "unsupported iconpart: " ++ show x

instance Pretty Icon where
  pretty (Icon parts) = braces $ text "Icon" <+> vsep (map pretty parts)

instance Pretty Jumper where
  pretty (Jumper c3) = braces $ text "Jumper" <+> pretty c3

instance Pretty SubModule where
  pretty (SubModule name c3) = braces $ text "SubModule:" <+> text name <+> pretty c3

instance Pretty ModPath where
  pretty (ModPath path file) = braces $ text "ModPath"
                               <+> text "filepath:" <> pretty path <> text "/" <> text file

instance Pretty Vdd where
  pretty (Vdd c) = braces $ text "Vdd" <+> pretty c
  
instance Pretty Direction where
  pretty x = braces $ text "Dir:" <+> (text $ show x)

instance Pretty Sig where
  pretty sig =
    let psig = case sig of
          SigSimple s -> text "SigSimple" <+> text s
          SigIndex s n -> text "SigIndex" <+> text s <+> pretty n
          SigHash s n -> text "SigHash" <+> text s <+> pretty n
          SigRange s from to -> text "SigRange" <+> text s
                                <+> (braces $ pretty from <> text ":" <> pretty to)
          SigRangeStep s from to step -> text "SigRangeStep" <+> text s
                                         <+> (braces
                                               $ pretty from
                                               <> text ":"
                                               <> pretty to
                                               <> text ":"
                                               <> pretty step)          
          SigQuote size val -> text "SigQuote"
                               <+> (braces (text "size:" <> pretty size)
                                     <+> (text "val:" <> pretty val))
          SigConcat sigs -> text "SigConcat" <+> hcat (map pretty sigs)
    in braces psig


instance Pretty Signal where
  pretty (Signal sig w dir) =
    let psig = case sig of
                 Nothing -> text "anon-sig"
                 Just s -> pretty s
        pdir = case dir of
                 Nothing -> text "no-dir"
                 Just dir -> pretty dir
    in braces $ text "Signal" <+> psig <+> pdir
instance Pretty Rot where
  pretty x = text (show x)

instance Pretty Coord3 where
  pretty (Coord3 x y r) = text "Coord3: " <+> pretty x <+> pretty y <+> pretty r

instance Pretty Point where
  pretty (Point x y) = braces $ text "Point:" <+> pretty x <+> pretty y 

instance Pretty Wire where
  pretty (Wire c Nothing) = text "<Wire" <+> pretty c <+> text "unnamed>" 
  pretty (Wire c (Just s)) = text "<Wire" <+> pretty c <+> pretty s <> text ">"

instance Pretty Coord5 where
  pretty (Coord5 x y r dx dy) =
    text "Coord5: " <+> pretty x <+> pretty y <+> pretty r <+> pretty dx <+> pretty dy

instance Pretty Port where
  pretty (Port c3 signal) =
    parens $ text "<Port:"
    <+> pretty c3
    <+> case signal of 
          Nothing -> text "anon"
          Just s -> pretty s

instance Pretty Part where
  pretty part =
    braces $ case part of
                 PortC x -> text "PortC" <+> pretty x
                 SubModuleC x -> text "SubModuleC" <+> pretty x
                 WireC x -> text "WireC" <+> pretty x
                 JumperC x -> text "JumperC" <+> pretty x
                 TermC x -> text "TermC" <+> pretty x
                 UnusedPart -> text "Unused part"

instance Pretty BoundingBox where
  pretty (BB l t r b) = braces $ text "BoundingBox:"
    <+> text "left:" <> pretty l 
    <+> text "right:" <> pretty r 
    <+> text "top:" <> pretty t 
    <+> text "bottom:" <> pretty b 

instance Pretty Power where
  pretty (Power vdd) = braces $ text "Power:" <+> pretty vdd

instance Pretty Thresholds where
  pretty (Thresholds vol vil vih voh) =
    braces $ text "Thresholds:"
    <+> text "vol:" <> pretty vol
    <+> text "vil:" <> pretty vil
    <+> text "vih:" <> pretty vih
    <+> text "voh:" <> pretty voh

instance Pretty Inputs where
  pretty (Inputs sigs) = braces $ text "Inputs:" <+> (list $ map pretty sigs)

instance Pretty Outputs where
  pretty (Outputs sigs) = braces $ text "Outputs:" <+> (list $ map pretty sigs)

instance Pretty Mode where
  pretty x = text $ show x

instance Pretty Duration where
  pretty x = braces $ text $ show x

instance Pretty Action where
  pretty action = case action of
    Assert s -> braces $ text "Assert:" <+> text s
    Deassert s -> braces $ text "Deassert:" <+> text s
    Sample s -> braces $ text "Sample:" <+> text s
    Tran dur -> braces $ text "Tran:" <+> pretty dur

instance Pretty CycleLine where
  pretty (CycleLine actions) = braces $ text "Action" <> (list $ map pretty actions)

instance Pretty BinVal where
  pretty x = text (show x)

instance Pretty TestLine where
  pretty (TestLine binvals comment) =    
    braces $ text "TestLine" <+> (hcat $ map pretty binvals) <+> fromMaybe empty (liftM pretty comment)

instance Pretty PlotDef where
  pretty (PlotDef sig xs) = braces $ text "PlotDef" <+> pretty sig <+> list (map pretty xs) 

instance Pretty PlotStyle where
  pretty (BinStyle sig) = braces $ text "BinStyle" <+> pretty sig
  pretty (HexStyle sig) = braces $ text "HexStyle" <+> pretty sig
  pretty (DecStyle sig) = braces $ text "DecStyle" <+> pretty sig
  pretty (SimplePlot sig) = braces $ text "SimplePlot" <+> pretty sig
  pretty (PlotDefStyle s sig) = braces $ text "PlotDefStyle" <+> pretty sig

instance Pretty ProcStage where
  pretty x = braces $ text "Stage: " <+> (text (show x))

instance Pretty ModTest where
  pretty mt = do
    let f select = braces $ fromMaybe empty $ liftM pretty (select mt)
    braces
      $ f modPower
      <$$> f modThresholds
      <$$> f modInputs
      <$$> f modOutputs
      <$$> f modMode
      <$$> f modCycleLine
      <$$> list (map pretty $ modTestLines mt)
      <$$> list (map pretty $ modPlotDef mt)
      <$$> list (map pretty $ modPlotStyles mt)

instance Pretty Module where
  pretty (Module name schem test icon) = braces
                                         $ text "Module:"
                                         <+> text name
                                         <+> pretty schem
                                         <+> pretty test
                                         <+> pretty icon

instance Pretty Schematic where
  pretty (Schematic parts) = braces $ text "Schematic:" <+> pretty parts

instance Pretty TopLevel where
  pretty (TopLevel s m) = braces
                          $    text "TopLevel:"
                          <+>  text "Stage: " <> pretty s
                          <$$> text "Mods:  " <> (list $ map pretty $ DM.toList m)
