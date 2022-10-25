{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.NutMeg
    ( NutMeg (..)
    , NutPlot (..)
    , NutWave (..)
    , NutPlotType (..)
    , NutField (..)
    , nutFieldName, nutFieldName'
    , bytesPerReal, bytesPerReal'
    , bytesPerComplex, bytesPerComplex'
    , popNutElement
    , readNutRealRow, readNutComplexRow
    , nutPlot, nutFlag, nutWave
    , nutRealWave, asRealVector
    -- , nutComplexWave, asComplexVector
    , flattenRealPlots
    , flattenComplexPlots
    , parseNutPlot
    , parseNutMeg
    , parseNutHeader
    , readNutRaw
    -- , encodeNutPlot
    ) where

import           Data.Maybe
import           Data.Int
import           Data.Complex
import           Data.Binary.Get
import qualified Data.Algorithms.KMP   as KMP
import qualified Data.Map.Strict       as M
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Vector.Unboxed   as V
import qualified Data.Matrix.Unboxed   as A

-- | NutMeg wave form data dypes
data NutWave = NutRealWave    {-# UNPACK #-} !(V.Vector Double)
             | NutComplexWave {-# UNPACK #-} !(V.Vector (Complex Double))
    deriving (Show, Eq)

nutWave :: NutWave -> Either (V.Vector (Complex Double)) (V.Vector Double)
nutWave (NutRealWave    nw) = Right nw
nutWave (NutComplexWave nw) = Left  nw

nutRealWave :: NutWave -> Maybe (V.Vector Double)
nutRealWave (NutRealWave nw) = Just nw
nutRealWave _                = Nothing

asRealVector :: NutWave -> V.Vector Double
asRealVector (NutRealWave nw) = nw
asRealVector _                = undefined

concatRealWaves :: [NutWave] -> NutWave
concatRealWaves = NutRealWave . V.concat . map asRealVector

joinRealWaves :: NutWave -> NutWave -> NutWave
joinRealWaves a b = concatRealWaves [a, b]

flattenRealPlots :: [NutPlot] -> NutPlot
flattenRealPlots nps = let { nutPlotName  = pn'
                           ; nutNumVars   = nv'
                           ; nutNumPoints = np'
                           ; nutVariables = vn'
                           ; nutPlotType  = pt'
                           ; nutData      = dt'
                           } in NutPlot { .. }
  where fp  = head nps
        pn' = nutPlotName fp
        nv' = nutNumVars fp
        np' = nutNumPoints fp
        vn' = nutVariables fp
        pt' = nutPlotType fp
        dt' = M.unionsWith joinRealWaves (map nutData nps)

-- nutComplexWave :: NutWave -> Maybe (V.Vector (Complex Double))
-- nutComplexWave (NutComplexWave nw) = Just nw
-- nutComplexWave _                   = Nothing
-- 
-- asComplexVector :: NutWave -> V.Vector (Complex Double)
-- asComplexVector (NutComplexWave nw) = nw
-- asComplexVector _                   = undefined
-- 
-- concatComplexWaves :: [NutWave] -> NutWave
-- concatComplexWaves = NutComplexWave . V.concat . map asComplexVector
-- 
-- joinComplexWaves :: NutWave -> NutWave -> NutWave
-- joinComplexWaves a b = concatComplexWaves [a, b]

flattenComplexPlots :: [NutPlot] -> NutPlot
flattenComplexPlots nps = let { nutPlotName  = pn'
                              ; nutNumVars   = nv'
                              ; nutNumPoints = np'
                              ; nutVariables = vn'
                              ; nutPlotType  = pt'
                              ; nutData      = dt'
                              } in NutPlot { .. }
  where fp  = head nps
        pn' = nutPlotName fp
        nv' = nutNumVars fp
        np' = nutNumPoints fp
        vn' = nutVariables fp
        pt' = nutPlotType fp
        dt' = M.unionsWith joinRealWaves (map nutData nps)

-- | Real and Complex NutMeg plots
data NutPlotType = NutRealPlot | NutComplexPlot
    deriving (Show, Eq)

-- | Convert a flag from file to plot type
nutFlag :: String -> NutPlotType
nutFlag "complex" = NutComplexPlot
nutFlag "real"    = NutRealPlot
nutFlag _         = undefined

-- | Data Fields in NutMeg Files
data NutField = NutTitle 
              | NutDate 
              | NutPlotname 
              | NutFlags 
              | NutNoVariables 
              | NutNoPoints 
              | NutVariables 
              | NutBinData
    deriving (Show, Eq, Enum, Ord, Bounded)

-- Alternaitve Show instance calling `nutFieldName`.
-- instance Show NutField where
--   show nfn = CS.unpack . nutFieldName $ nfn

-- | Translate fieldname to ByteString representation in NutMeg file.
nutFieldName :: NutField -> BS.ByteString
nutFieldName NutTitle       = "Title:"
nutFieldName NutDate        = "Date:"
nutFieldName NutPlotname    = "Plotname:"
nutFieldName NutFlags       = "Flags:"
nutFieldName NutNoVariables = "No. Variables:"
nutFieldName NutNoPoints    = "No. Points:"
nutFieldName NutVariables   = "Variables:"
nutFieldName NutBinData     = "Binary:"

-- | Convenient Transformer to Lazy ByteString
nutFieldName' :: NutField -> BL.ByteString
nutFieldName' = BL.fromStrict . nutFieldName

-- | A 'plot' as it is represented within the NutMeg File format.
data NutPlot = NutPlot { nutPlotName  :: String
                       , nutNumVars   :: Int
                       , nutNumPoints :: Int
                       , nutVariables :: [String]
                       , nutPlotType  :: NutPlotType
                       , nutData      :: !(M.Map String NutWave) }
--              | ComplexPlot { nutPlotName    :: String
--                            , nutNumVars     :: Int
--                            , nutNumPoints   :: Int
--                            , nutVariables   :: [String]
--                            , nutPlotType    :: NutPlotType
--                            , complexNutData :: !(M.Map String NutWave) }
    deriving (Show)

-- | Representation of NutMeg file contents, where plot names are mapped to
-- corresponding plot types and data.
data NutMeg = NutMeg { nutTitle :: String
                     , nutDate  :: String
                     , nutPlots :: !(M.Map String NutPlot)}
    deriving (Show)

-- | How many bytes per real data point
bytesPerReal :: Int
bytesPerReal = 8
-- | Convenient transformation to Int64 for certain calculations
bytesPerReal' :: Int64
bytesPerReal' = fromIntegral bytesPerReal

-- | How many bytes per complex data point
bytesPerComplex :: Int
bytesPerComplex = 2 * bytesPerReal
-- | Convenient transformation to Int64 for certain calculations
bytesPerComplex' :: Int64
bytesPerComplex' = fromIntegral bytesPerReal

-- | Convenience function for reading a NutMeg File
readNutRaw :: FilePath -> IO BS.ByteString
readNutRaw = BS.readFile

-- | Reads a line from a list of ByteStrings, that starts with a given prefix.
readNutElement' :: BS.ByteString -> [BS.ByteString] -> Maybe String
readNutElement' eid bs = CS.unpack . CS.strip <$> elem' bs
    where elem' = CS.stripPrefix eid . head . filter (BS.isPrefixOf eid)

-- | Takes a `NutField` name and returns the correspinding value.
readNutElement :: NutField -> BS.ByteString -> String
readNutElement nf bs = fromJust $ readNutElement' nfn bls
    where nfn = nutFieldName nf 
          bls = CS.lines bs

-- | Read a row of real valued data
readNutRealRow :: Int64 -> Int -> BL.ByteString -> V.Vector Double
readNutRealRow n o = V.fromList . runGet (skip (o * bytesPerReal) >> decoder [])
    where 
        o' = fromIntegral (o * bytesPerReal)
        n' = n * bytesPerReal'
        decoder !acc =  do
            !br <- bytesRead
            !be <- isEmpty
            if (br >= (n' + o')) || be
               then return $! reverse acc
               else getDoublebe >>= decoder . (:acc)

-- | Read a row of complex valued data
readNutComplexRow :: Int64 -> Int -> BL.ByteString -> V.Vector (Complex Double)
readNutComplexRow n o = V.fromList . runGet (skip (o * bytesPerComplex) >> decoder)
    where 
        o' = fromIntegral (o * bytesPerComplex)
        n' = n * bytesPerComplex'
        decoder =  do
            br <- bytesRead
            be <- isEmpty
            if (br >= (2 * n' + o')) || be
               then return []
               else do !r  <- getDoublebe
                       !c  <- getDoublebe
                       !ds <- decoder
                       return $! ( (r:+c) : ds )

-- | Construct NutPlot type
nutPlot :: String -> NutPlotType -> Int -> Int -> [String] -> BL.ByteString -> NutPlot
nutPlot pn nt np nv vn bs 
        = NutPlot { nutPlotName  = pn
                  , nutNumVars   = nv
                  , nutNumPoints = np
                  , nutVariables = vn
                  , nutPlotType  = nt
                  , nutData      = M.fromList $ zip vn dt }
    where 
      !dt = if nt == NutRealPlot 
               then map NutRealWave    . A.toColumns . A.fromRows 
                                       $ [rr (j * nv) | j <- [ 0 .. (np - 1) ]]
               else map NutComplexWave . A.toColumns . A.fromRows 
                                       $ [rc (j * nv) | j <- [ 0 .. (np - 1) ]]
      rr :: Int -> V.Vector Double
      rr i = readNutRealRow    (fromIntegral nv) (fromIntegral i) bs
      rc :: Int -> V.Vector (Complex Double)
      rc i = readNutComplexRow (fromIntegral nv) (fromIntegral i) bs

-- | Get the first NutMeg Element and the Rest
popNutElement :: NutField -> BS.ByteString -> (Maybe BS.ByteString, BS.ByteString)
popNutElement field dat = (elem'', rest)
    where nextField = succ field
          (elem', rest) = BS.breakSubstring (nutFieldName nextField) dat
          elem''        = CS.strip <$> BS.stripPrefix (nutFieldName field) elem'

-- | Extract NutMeg header information
parseNutHeader :: BS.ByteString -> [String]
parseNutHeader = ph NutTitle
    where ph nf bs | BS.null bs = []
                   | (nf == NutTitle) || (nf == NutDate) = 
                       let (e, r) = popNutElement nf bs
                        in (CS.unpack . fromJust $ e) : ph (succ nf) r
                   | otherwise = undefined

-- | Extract NutPlot header information.
parseNutPlotHeader :: BS.ByteString -> [String]
parseNutPlotHeader = ph NutPlotname
    where ph nf bs | nf `elem` [ NutPlotname .. NutNoPoints ] =
                       let (e, r) = popNutElement nf bs
                        in (CS.unpack . fromJust $ e) : ph (succ nf) r
                   | otherwise = []

-- | Returns a NutPlot for a given ByteString segment.
parseNutPlot :: BS.ByteString -> NutPlot
parseNutPlot plt = nutPlot pn fl np nv vn dt
    where
      (hdr, dat) = BS.breakSubstring (nutFieldName NutBinData) plt
      hdr'       = parseNutPlotHeader hdr
      pn         = head hdr'
      fl         = nutFlag (hdr' !! 1)
      nv         = read (hdr' !! 2) :: Int
      np         = max 1 $ read (hdr' !! 3) :: Int
      vr         = snd . BS.breakSubstring "\t" . snd 
                 . BS.breakSubstring (nutFieldName NutVariables) $ hdr
      vn         = map (CS.unpack . (!!1) . CS.words) $ CS.lines vr
      dt         = BL.fromStrict . fromJust . BS.stripPrefix "Binary:\n" $ dat

-- | Split a ByteString read from nutmeg file into NutPlots
splitNutString :: [Int] -> BS.ByteString -> [NutPlot]
splitNutString [ ]       _   = []
splitNutString [a]       str = [plt]
  where
    !b    = BS.length str
    !plt  = parseNutPlot . BS.take (b - a) . BS.drop a $! str
splitNutString (a:b:idx) str = plt : splitNutString idx' str
  where
    !plt  = parseNutPlot . BS.take (b - a) . BS.drop a $! str
    !idx' = b:idx

-- | Returns a NutMeg for given file contents.
parseNutMeg :: BS.ByteString -> NutMeg
parseNutMeg !nut = NutMeg { nutTitle = nt
                          , nutDate  = nd
                          , nutPlots = mps }
    where 
      (hdr, bdy) = BS.breakSubstring (nutFieldName NutPlotname) nut
      nt         = readNutElement NutTitle hdr
      nd         = readNutElement NutDate hdr
      pn         = KMP.build . BS.unpack $! nutFieldName NutPlotname
      !idx       = KMP.match pn $! BS.unpack bdy
      !nps       = splitNutString idx bdy
      !mps       = M.fromList $! zip (map nutPlotName nps) nps

-- encodeNutPlot :: NutPlot -> ByteString
