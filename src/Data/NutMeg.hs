{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read Binary NutMeg Data
module Data.NutMeg ( -- * Data Types
                     NutMeg, Plot, Wave (..), Flag (..), Field (..)
                   -- * Read raw binary data
                   , readFile, readFile', slurp
                   -- * Parsing NutMeg binary data
                   , extractPlot, extractPlot', parseHeader, readField
                   -- * Accessing Plot data
                   , asVector, flattenPlots, flattenPlots'
                   -- * Utilities
                   , r2c, concat
                   ) where

import           GHC.Generics
import           Unsafe.Coerce                    (unsafeCoerce)
import           Control.Monad
import           Control.DeepSeq
import           Data.Int                         (Int64)
import           Data.Maybe                       (fromJust)
import           Data.Complex
import           Data.ByteString.Lazy             (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.ByteString.Internal         (c2w, isSpaceWord8)
import           Data.Binary.Get
import           Data.Map                         (Map)
import qualified Data.Map                   as M
import           Data.Vector.Unboxed              (Unbox, Vector, (!), (++))
import qualified Data.Vector.Unboxed        as V

import           Prelude                          hiding (readFile, (++), concat)

-- | Data Field identifier in NutMeg (binary) Files
data Field = Title       -- ^ Title of the @'NutMeg'@ file, usually first line of netlist
           | Date        -- ^ Date of creation
           | Plotname    -- ^ Name of a @'Plot'@
           | Flags       -- ^ Whether the @'Plot'@ is @'Real'@ or @'Complex'@
           | NoVariables -- ^ No. Variables in the @'Plot'@
           | NoPoints    -- ^ No. Points per Variable
           | Variables   -- ^ String representation of Variables in the @'Plot'@
           | Binary      -- ^ Binary Data, 'Double' or 'Complex Double' encoded in Big Endian
    deriving (Eq, Enum, Ord, Bounded, Generic, NFData)

instance Show Field where
  show Title       = "Title:"
  show Date        = "Date:"
  show Plotname    = "Plotname:"
  show Flags       = "Flags:"
  show NoVariables = "No. Variables:"
  show NoPoints    = "No. Points:"
  show Variables   = "Variables:"
  show Binary      = "Binary:"

-- | Flag indicating whether a plot is real or complex valued
data Flag = Real    -- ^ Real valued ('Double') plot
          | Complex -- ^ Complex valued ('Complex Double') plot
    deriving (Eq, Bounded, Generic)

instance Read Flag where
  readsPrec _ "real"    = [(Real, "")]
  readsPrec _ "complex" = [(Complex, "")]
  readsPrec _ _         = undefined

instance Show Flag where
  show Real    = "real"
  show Complex = "complex"

-- | Wrapper around Real or Complex valued Vector, so they can be in the same
-- 'List'.
data Wave = RealWave    {-# UNPACK #-} !(Vector Double)           -- ^ Real valued ('Double') wave form
          | ComplexWave {-# UNPACK #-} !(Vector (Complex Double)) -- ^ Complex valued ('Complex Double') wave form
    deriving (Show, Eq, Generic, NFData)

-- | A /Plot/ inside a @'NutMeg'@ file consists of uniquely identified waveforms:
-- @[(Variable Name, Waveform)]@
type Plot = Map String Wave

-- | A NutMeg file consists of a list of @'Plot'@s
-- @[(Plotname, @'Plot'@)]
-- Plotnames do /not/ have to be unique in a NutMeg file. Data may be lost by
-- turning this into a 'Map'.
type NutMeg = [(String, Plot)]

-- | Number of bytes per @'Double'@
bytesPerReal :: Int64
bytesPerReal = 8

-- | Transpose Rows / Columns
r2c :: (Unbox a) => Int -> Int -> Vector a -> [Vector a]
r2c numVars numPoints wave' = waves
  where
    r2c' v = V.fromList $ map ((wave' !) . (+ v)) [0, numVars .. numPoints * 2 - 1]
    waves  = map r2c' [0 .. numVars - 1]

-- | Convert Waveform to unboxed 'Vector'
asVector :: (Unbox a) => Wave -> Vector a
asVector (ComplexWave w) = unsafeCoerce w
asVector (RealWave w)    = unsafeCoerce w

-- | Joins two @'Wave'@s of the same type: @wave1 ++ wave2@
-- Attempting to concatenate a Real and Complex wave will result in an error
concat :: Wave -> Wave -> Wave
concat (RealWave a)    (RealWave b)    = RealWave    (a ++ b)
concat (ComplexWave a) (ComplexWave b) = ComplexWave (a ++ b)
concat _               _               = error "Cannot concatenate Real and Complex Waves"

-- | Concatenate waves of all @'Plot'@s in @'NutMeg'@. Probably won't work as intended
-- when variable names of the @'Plot'@s don't line up
flattenPlots' :: NutMeg -> Plot
flattenPlots' = M.unionsWith concat . map snd

-- | Concatenate the @'Wave'@s of a given list of @'Plot'@ names. This will
-- only work if the keys line up.
flattenPlots :: [String] -> NutMeg -> Plot
flattenPlots plotNames nut = flattenPlots' $ filter ((`elem` plotNames) . fst) nut

-- | Read a @'NutMeg'@ field from a ByteString
readField :: Field -> ByteString -> String
readField nf bs = CL.unpack . BL.dropWhile isSpaceWord8 . fromJust $ BL.stripPrefix nfn bs
  where 
    nfn = CL.pack $ show nf

-- | Extract @'Plot'@ header information:
-- @((Plotname, 'Flag', No. Variables, No. Points), [Variable Names])@
parseHeader :: [ByteString] -> ((String, Flag, Int, Int),  [String])
parseHeader hdr = (h, var')
  where
    hdr' = zipWith readField [ Plotname .. NoPoints ] hdr
    h    = (head hdr', read (hdr' !! 1) , read (hdr' !! 2) , read (hdr' !! 3))
    vars = BL.cons (c2w '\t')
                   (fromJust (BL.stripPrefix (CL.pack $ show Variables) (hdr !! 4)))
         : drop 5 hdr
    var' = map (CL.unpack . ve) vars
    ve s = BL.takeWhile (/= c2w '\t') $ BL.drop (succ . (!! 2)
         . BL.elemIndices (c2w '\t') $ s) s

-- | Extract the wave forms from binary data given header information
extractPlot' :: Flag       -- ^ Real or Complex Data
             -> Int        -- ^ No. Variables
             -> Int        -- ^ No. Points
             -> ByteString -- ^ Binary Data
             -> [Wave]     -- ^ Wave forms
extractPlot' Real    numVars numPoints bin = waves
  where
    wave' = V.generate (numVars * numPoints) 
          $ \i -> runGet getDoublebe $ BL.drop (fromIntegral i * bytesPerReal) bin
    waves = map RealWave $ r2c numVars numPoints wave'
extractPlot' Complex numVars numPoints bin = waves
  where
    wave' = V.generate (numVars * numPoints)
          $ \i -> let i'   = fromIntegral $ i * 2
                      real = runGet getDoublebe $ BL.drop (i' * bytesPerReal) bin
                      imag = runGet getDoublebe $ BL.drop ((i' + 1) * bytesPerReal) bin
                   in (real:+imag)
    waves = map ComplexWave $ r2c numVars numPoints wave'

-- | Read The first plot encountered in ByteString String:
-- @((Plotname, 'Plot'), Remianing ByteString)@
extractPlot :: ByteString -> ((String, Plot), ByteString)
extractPlot bs = ((plotName, plot) , rest)
  where
    hdr  = takeWhile (not . BL.isPrefixOf "Binary:") $ CL.lines bs
    ((plotName, flag, numVars, numPoints), varNames) = parseHeader hdr
    n    = sum (map BL.length hdr) + 8 + fromIntegral (length hdr)
    n'   = fromIntegral $ numVars * numPoints * 8
    bin  = BL.drop n bs
    !plot = M.fromList . zip varNames $! extractPlot' flag numVars numPoints bin
    !rest = BL.drop n' bs

-- | Read all @'Plot'@s encountered in a ByteString
slurp :: ByteString -> NutMeg
slurp !bs | BL.null bs                   = []
          | BL.isPrefixOf "Plotname:" bs = let (plot, rest) = extractPlot bs
                                            in plot : slurp rest
          | otherwise                    = slurp $ BL.drop 1 bs

-- | Read a binary nutmeg .raw file
readFile :: FilePath -> IO NutMeg
readFile path = slurp . CL.unlines . drop 2 . CL.lines <$!> BL.readFile path

-- | Strict version of @'readFile'@
readFile' :: FilePath -> IO NutMeg
readFile' path = force <$> readFile path
