{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read Binary Nutmeg Data
module Data.NutMeg ( -- * Data Types
                     NutMeg (..)
                   , NutPlot (..)
                   , NutWave (..)
                   , NutPlotType (..)
                   , NutField (..)
                   -- * Reading and Parsing Nutmeg Binary
                   , readNutRaw, readNutRaw'
                   , parseNutPlot, parseNutPlots
                   , parseNutMeg, parseNutMeg'
                   -- * Misc
                   , nutFieldName, nutPlot, nutFlag, nutWave
                   -- * Waveform access
                   , nutRealWave, asRealVector
                   , nutComplexWave, asComplexVector
                   -- * Read raw binary data
                   , readRealBinary, readComplexBinary
                   , concatComplexWaves, joinComplexWaves
                   , flattenRealPlots, flattenComplexPlots
                   -- , encodeNutPlot
                   ) where

import           GHC.Generics
import           Control.DeepSeq
import           Data.Maybe
import           Data.Int
import           Data.List                        (transpose)
import           Data.List.Split                  (chunksOf, splitOn)
import           Data.Complex
import           Data.ByteString.Internal         (c2w, isSpaceWord8)
import           Data.Binary.Get
import qualified Data.Map.Strict            as M
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Vector.Unboxed        as V

-- | NutMeg wave form data dypes
data NutWave = NutRealWave    {-# UNPACK #-} !(V.Vector Double)           -- ^ Real valued (Double) wave form
             | NutComplexWave {-# UNPACK #-} !(V.Vector (Complex Double)) -- ^ Complex valued (Complex Double) wave form
    deriving (Show, Eq, Generic, NFData)

-- | Convert a nut wave to either real or complex valued Vector
nutWave :: NutWave -> Either (V.Vector (Complex Double)) (V.Vector Double)
nutWave (NutRealWave    nw) = Right nw
nutWave (NutComplexWave nw) = Left  nw

-- | Convert a nut wave to a Real valued Vector
nutRealWave :: NutWave -> Maybe (V.Vector Double)
nutRealWave (NutRealWave nw) = Just nw
nutRealWave _                = Nothing

-- | Convert a nut wave to a Real valued Vector
asRealVector :: NutWave -> V.Vector Double
asRealVector (NutRealWave nw) = nw
asRealVector _                = undefined

-- | Concatenate real waves
concatRealWaves :: [NutWave] -> NutWave
concatRealWaves = NutRealWave . V.concat . map asRealVector

-- | Convenience for zipping real waves
joinRealWaves :: NutWave -> NutWave -> NutWave
joinRealWaves a b = concatRealWaves [a, b]

-- | Concatenate Data of plots with same variable names
flattenRealPlots :: [NutPlot] -> NutPlot
flattenRealPlots nps = let { nutPlotName  = pn'
                           ; nutNumVars   = nv'
                           ; nutNumPoints = np'
                           ; nutVariables = vn'
                           ; nutPlotType  = pt'
                           ; nutData      = dt'
                           } in NutPlot { .. }
  where 
    fp  = head nps
    pn' = nutPlotName fp
    nv' = nutNumVars fp
    np' = nutNumPoints fp
    vn' = nutVariables fp
    pt' = nutPlotType fp
    dt' = M.unionsWith joinRealWaves (map nutData nps)

-- | Convert to Complex valued Vector
nutComplexWave :: NutWave -> Maybe (V.Vector (Complex Double))
nutComplexWave (NutComplexWave nw) = Just nw
nutComplexWave _                   = Nothing

-- | Convert to Complex valued Vector
asComplexVector :: NutWave -> V.Vector (Complex Double)
asComplexVector (NutComplexWave nw) = nw
asComplexVector _                   = undefined

-- | Concatenate complex nut waves
concatComplexWaves :: [NutWave] -> NutWave
concatComplexWaves = NutComplexWave . V.concat . map asComplexVector

-- | Convenience for zipping complex waves
joinComplexWaves :: NutWave -> NutWave -> NutWave
joinComplexWaves a b = concatComplexWaves [a, b]

-- | Flatten plots with same variable names
flattenComplexPlots :: [NutPlot] -> NutPlot
flattenComplexPlots nps = let { nutPlotName  = pn'
                              ; nutNumVars   = nv'
                              ; nutNumPoints = np'
                              ; nutVariables = vn'
                              ; nutPlotType  = pt'
                              ; nutData      = dt'
                              } in NutPlot { .. }
  where 
    fp  = head nps
    pn' = nutPlotName fp
    nv' = nutNumVars fp
    np' = nutNumPoints fp
    vn' = nutVariables fp
    pt' = nutPlotType fp
    dt' = M.unionsWith joinRealWaves (map nutData nps)

-- | Real and Complex NutMeg plots
data NutPlotType = NutRealPlot | NutComplexPlot
    deriving (Show, Eq, Generic, NFData)

-- | Convert a flag from file to plot type
nutFlag :: String -> NutPlotType
nutFlag "complex" = NutComplexPlot
nutFlag "real"    = NutRealPlot
nutFlag _         = undefined

-- | Data Fields in NutMeg Files
data NutField = NutTitle        -- ^ Title of the plot, usually first line of netlist
              | NutDate         -- ^ Date of the plot
              | NutPlotname     -- ^ Name of plot
              | NutFlags        -- ^ Real or Complex
              | NutNoVariables  -- ^ No. Variables
              | NutNoPoints     -- ^ No. Points per Variable
              | NutVariables    -- ^ String representation of Variables
              | NutBinData      -- ^ Binary Data, Double or Complex Double in Big Endian
    deriving (Show, Eq, Enum, Ord, Bounded, Generic, NFData)

-- Alternaitve Show instance calling `nutFieldName`.
-- instance Show NutField where
--   show nfn = CS.unpack . nutFieldName $ nfn

-- | Translate fieldname to ByteString representation in NutMeg file.
nutFieldName :: NutField -> BL.ByteString
nutFieldName NutTitle       = "Title:"
nutFieldName NutDate        = "Date:"
nutFieldName NutPlotname    = "Plotname:"
nutFieldName NutFlags       = "Flags:"
nutFieldName NutNoVariables = "No. Variables:"
nutFieldName NutNoPoints    = "No. Points:"
nutFieldName NutVariables   = "Variables:"
nutFieldName NutBinData     = "Binary:"

-- | Convenient Transformer to Lazy ByteString
-- nutFieldName' :: NutField -> BL.ByteString
-- nutFieldName' = BL.fromStrict . nutFieldName

-- | A 'plot' as it is represented within the NutMeg File format.
data NutPlot = NutPlot { nutPlotName  :: !String                    -- ^ Plot Name
                       , nutNumVars   :: !Int                       -- ^ Number of Variables
                       , nutNumPoints :: !Int                       -- ^ Number of data points per Variable
                       , nutVariables :: ![String]                  -- ^ Names of Variables
                       , nutPlotType  :: !NutPlotType               -- ^ Real or Complex
                       , nutData      :: !(M.Map String NutWave)    -- ^ Waveforms
                       } deriving (Show, Generic, NFData)

-- | Representation of NutMeg file contents, where plot names are mapped to
-- corresponding plot types and data.
data NutMeg = NutMeg { nutTitle :: !String                  -- ^ Title of Plot
                     , nutDate  :: !String                  -- ^ Data of Plot
                     , nutPlots :: !(M.Map String NutPlot)  -- ^ Plots contained within
                     } deriving (Show, Generic, NFData)

-- | Convenient transformation to Int64 for certain calculations
bytesPerReal :: Int64
bytesPerReal = 8

-- | How many bytes per complex data point
bytesPerComplex :: Int64
bytesPerComplex = 2 * bytesPerReal

-- | Trim White spaces
trim :: BL.ByteString -> BL.ByteString
trim = BL.reverse . BL.dropWhile isSpaceWord8
     . BL.reverse . BL.dropWhile isSpaceWord8

-- | Takes a `NutField` name and returns the correspinding value.
readNutElement :: NutField -> BL.ByteString -> String
readNutElement nf bs = CL.unpack . trim . fromJust $ BL.stripPrefix nfn bs
  where 
    nfn = nutFieldName nf 

-- | Read block of real data
readRealBinary :: BL.ByteString -> [Double]
readRealBinary string | BL.length string < bytesPerReal = []
                      | otherwise = value : readRealBinary string'
  where
    string' = BL.drop bytesPerReal string
    value   = runGet getDoublebe string

-- | Read block of complex data
readComplexBinary :: BL.ByteString -> [Complex Double]
readComplexBinary string | BL.length string < bytesPerComplex = []
                         | otherwise = value : readComplexBinary string'
  where
    string' = BL.drop bytesPerComplex string
    real    = runGet getDoublebe string
    imag    = runGet getDoublebe $ BL.drop bytesPerReal string
    value   = real :+ imag

-- | Construct NutPlot type
nutPlot :: String -> NutPlotType -> Int -> Int -> [String] -> BL.ByteString
        -> NutPlot
nutPlot pn nt np nv vn !bs = NutPlot { nutPlotName  = pn
                                     , nutNumVars   = nv
                                     , nutNumPoints = np
                                     , nutVariables = vn
                                     , nutPlotType  = nt
                                     , nutData      = M.fromList $ zip vn dt }
  where 
    dt = if nt == NutRealPlot
            then map (NutRealWave . V.fromList)    . transpose . chunksOf nv
                                                   $ readRealBinary bs
            else map (NutComplexWave . V.fromList) . transpose
                                     . chunksOf nv $ readComplexBinary bs

-- | Extract NutPlot header information.
parseNutPlotHeader :: [BL.ByteString] -> ([String], [String])
parseNutPlotHeader hdr = (hdr', var')
  where
    hdr' = zipWith readNutElement [ NutPlotname .. NutNoPoints ] hdr
    vars = BL.cons (c2w '\t')
                   (fromJust (BL.stripPrefix (nutFieldName NutVariables) (hdr !! 4)))
         : drop 5 hdr
    var' = map (CL.unpack . ve) vars
    ve s = BL.takeWhile (/= c2w '\t') $ BL.drop (succ . (!! 2)
         . BL.elemIndices (c2w '\t') $ s) s

-- | Returns a NutPlot for a given ByteString segment.
parseNutPlot :: [BL.ByteString] -> NutPlot
parseNutPlot plt = nutPlot pn fl np nv vn dt
  where
    (hdr',dat') = break (BL.isPrefixOf (nutFieldName NutBinData)) plt
    (hf,vn)     = parseNutPlotHeader hdr'
    pn          = head hf
    fl          = nutFlag (hf !! 1)
    nv          = read (hf !! 2)
    np          = max 1 $ read (hf !! 3)
    dt          = BL.reverse . BL.drop 1 . BL.reverse . CL.unlines . drop 1 $ dat'

-- | Parse all plots contained within a nutmeg file after title and date
parseNutPlots :: [BL.ByteString] -> [NutPlot]
parseNutPlots []  = []
parseNutPlots bdy = parseNutPlot plt' : parseNutPlots bdy'
  where
    ph         = take 2 bdy
    (plt,rst') = break (BL.isPrefixOf (nutFieldName NutFlags)) $ drop 2 bdy
    pt'        = map BL.pack $ splitOn (BL.unpack (nutFieldName NutPlotname))
               $ BL.unpack (last plt)
    nh         = BL.concat [nutFieldName NutPlotname, last pt']
    plt'       = ph ++ init plt ++ [head pt']
    bdy'       = if null rst' || length pt' < 2 then [] else nh : rst'

-- | Parse nutmeg content with offset and return new offset
parseNutMeg' :: [BL.ByteString] -> (NutMeg, Int64)
parseNutMeg' (nt':nd':bdy') = (meg, off) 
  where 
    nt   = readNutElement NutTitle nt'
    nd   = readNutElement NutDate  nd'
    !nps = parseNutPlots bdy'
    !mps = M.fromList $! zip (map nutPlotName nps) nps
    !meg = NutMeg { nutTitle = nt
                  , nutDate  = nd
                  , nutPlots = mps }
    !off = 0

parseNutMeg' _ = error "Empty File or Wrong Format"

-- | Parse Nutmeg content 
parseNutMeg :: BL.ByteString -> NutMeg
parseNutMeg = fst . parseNutMeg' . CL.lines

-- | Convenience function for reading a NutMeg File
readNutRaw :: FilePath -> IO BL.ByteString
readNutRaw = BL.readFile

-- | Read and Prase NutMeg
readNutRaw' :: FilePath -> IO NutMeg
readNutRaw' p = parseNutMeg <$> readNutRaw p

-- encodeNutPlot :: NutPlot -> ByteString
