{-# OPTIONS_GHC -Wall -fno-full-laziness #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read Binary NutMeg Data
module Data.NutMeg ( -- * Data Types
                     NutMeg, Plot, RealPlot, ComplexPlot, Wave (..), Flag (..), Field (..)
                   -- * Read raw binary data
                   , readFile
                   -- * Parsing NutMeg binary data
                   , extractPlots, extractPlot, parseHeader, readField
                   -- * Accessing Plot data
                   , asVector, vectorize, flattenPlots, flattenPlots'
                   , asRealPlot, asComplexPlot
                   -- * Utilities
                   , concat, isReal, isComplex, isReal', isComplex'
                   , byteSwap, castByteStringToVector
                   ) where

import           GHC.Generics
import           Control.DeepSeq
import           Control.Monad                    ((<$!>))
import           Data.Either
import           Data.Maybe                       (fromJust)
import           Data.Complex
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as CS
import           Data.ByteString.Internal         (c2w, isSpaceWord8, unsafePackLenBytes, toForeignPtr)
import           Data.ByteString.Unsafe           (unsafeIndex)
import           Data.Map                         (Map)
import qualified Data.Map                   as M
import           Data.Vector.Storable             (Storable, Vector, (!), (++))
import qualified Data.Vector.Storable       as V
import           Prelude                          hiding (readFile, (++), concat)
import           Foreign.ForeignPtr               (ForeignPtr, castForeignPtr)

-- | Swap Bytes of Big-Endian encoded ByteString
-- Thanks Noughtmare:
-- https://stackoverflow.com/a/71341067
byteSwap :: BS.ByteString -> BS.ByteString
byteSwap !xs = unsafePackLenBytes (len `quot` bytesPerReal * bytesPerReal)
                                   [ unsafeIndex xs $ i * bytesPerReal + j
                                   | i <- [0 .. len `quot` bytesPerReal - 1]
                                   , j <- [bytesPerReal - 1, bytesPerReal - 2 .. 0] ]
  where
    !len = fromIntegral $! BS.length xs

-- | Swap bytes and  cast to vector (slow)
castByteStringToVector :: (Storable a) => BS.ByteString -> Vector a
castByteStringToVector !xs = V.unsafeCast ys'
  where
    (ptr, off, len) = toForeignPtr xs
    xs' = V.unsafeFromForeignPtr ptr off len
    ys' = V.concat [ V.reverse $ V.unsafeSlice idx bytesPerReal xs'
                   | idx <- [0, bytesPerReal .. len - 1] ]

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
data Flag = Real'    -- ^ Real valued ('Double') plot
          | Complex' -- ^ Complex valued ('Complex Double') plot
    deriving (Eq, Bounded, Generic, NFData)

instance Read Flag where
  readsPrec _ "real"    = [(Real', "")]
  readsPrec _ "complex" = [(Complex', "")]
  readsPrec _ _         = undefined

instance Show Flag where
  show Real'    = "real"
  show Complex' = "complex"

-- | Wrapper around Real or Complex valued Vector, so they can be stored in the
-- same List.
data Wave = RealWave    {-# UNPACK #-} !(Vector Double)           -- ^ Real valued ('Double') wave form
          | ComplexWave {-# UNPACK #-} !(Vector (Complex Double)) -- ^ Complex valued ('Complex Double') wave form
    deriving (Show, Eq, Generic, NFData)

-- | A /Plot/ inside a @'NutMeg'@ file consists of uniquely identified waveforms:
-- @[(Variable Name, Waveform)]@
type Plot        = Map String Wave

-- | Type alias for real valued @'Plot'@
type RealPlot    = Map String (Vector Double)

-- | Type alias for complex valued @'Plot'@
type ComplexPlot = Map String (Vector (Complex Double))

-- | A NutMeg file consists of a list of @'Plot'@s
-- @[(Plotname, @'Plot'@)]
-- Plotnames do /not/ have to be unique in a NutMeg file. Data may be lost by
-- turning this into a 'Map'.
type NutMeg = [(String, Plot)]

-- | Number of bytes per @'Double'@
-- bytesPerReal :: Int64
bytesPerReal :: Int
bytesPerReal = 8

-- | Transpose Rows / Columns
r2c :: (Storable a) => Int -> Int -> Vector a -> [Vector a]
r2c numVars numPoints !wave' = waves
  where
    r2c' v = V.fromList $ map ((wave' !) . (+ v)) [0, numVars .. numPoints * numVars - 1]
    waves  = map r2c' [0 .. numVars - 1]

-- | Check whether waveform is real valued
isReal' :: Wave -> Bool
isReal' (RealWave _) = True
isReal' _            = False

-- | Check whether waveform is complex valued
isComplex' :: Wave -> Bool
isComplex' = not . isReal'

-- | Check whether Plot is real valued
isReal :: Plot -> Bool
isReal = all isReal' . M.elems

-- | Check whether Plot is complex valued
isComplex :: Plot -> Bool
isComplex = not . isReal

-- | Convert Waveform to unboxed 'Vector', fails horribly if types are incorrect
asVector :: Wave -> Either (Vector (Complex Double)) (Vector Double)
asVector (RealWave    w) = Right w
asVector (ComplexWave w) = Left  w

-- | Get rid of @'Wave'@ type and convert to either 'Complex Double' or
-- 'Double' Vector, depending on Wave type.
vectorize :: Plot -> Either ComplexPlot RealPlot
vectorize p | M.null complexPlots = Right realPlots
            | otherwise           = Left complexPlots
  where
    (complexPlots, realPlots) = M.mapEither asVector p

-- | Unsafe extraction of 'Right' value for real valued plots. Check with
-- @'isReal'@ before using, to be sure
asRealPlot :: Plot -> RealPlot
asRealPlot plot = plot'
  where
    (Right plot') = vectorize plot

-- | Unsafe extraction of 'Left' value for complex valued plots. Check with
-- @'isComplex'@ before using, to be sure
asComplexPlot :: Plot -> ComplexPlot
asComplexPlot plot = plot'
  where
    (Left plot') = vectorize plot

-- | Joins two @'Wave'@s of the same type: @wave1 ++ wave2@
-- Attempting to concatenate a Real and Complex wave will result in an error
concat :: Wave -> Wave -> Wave
concat (RealWave    a) (RealWave    b) = RealWave    (a ++ b)
concat (ComplexWave a) (ComplexWave b) = ComplexWave (a ++ b)
concat _               _               = error "Cannot concatenate Real and Complex Waves"

-- | Concatenate waves of all @'Plot'@s in @'NutMeg'@. Probably won't work as intended
-- when variable names of the @'Plot'@s don't line up
flattenPlots' :: NutMeg -> Plot
flattenPlots' = M.unionsWith concat . map snd

-- | Concatenate the @'Wave'@s of a given list of @'Plot'@ names. This will
-- only work if the keys line up.
flattenPlots :: [String] -> NutMeg -> Plot
flattenPlots !plotNames !nut = flattenPlots' $ filter ((`elem` plotNames) . fst) nut

-- | Read a @'NutMeg'@ field from a ByteString
readField :: Field -> ByteString -> String
readField !nf !bs = CS.unpack . BS.dropWhile isSpaceWord8 . fromJust $ BS.stripPrefix nfn bs
  where 
    nfn = CS.pack $ show nf

-- | Extract @'Plot'@ header information:
-- @((Plotname, 'Flag', No. Variables, No. Points), [Variable Names])@
parseHeader :: [ByteString] -> ((String, Flag, Int, Int),  [String])
parseHeader !hdr = (h, var')
  where
    hdr' = zipWith readField [ Plotname .. NoPoints ] hdr
    h    = ( head hdr'
           , read (hdr' !! 1)
           , max 1 $ read (hdr' !! 2)
           , max 1 $ read (hdr' !! 3) )
    vars = BS.cons (c2w '\t')
                   (fromJust (BS.stripPrefix (CS.pack $ show Variables) (hdr !! 4)))
         : drop 5 hdr
    var' = map (CS.unpack . ve) vars
    ve s = BS.takeWhile (/= c2w '\t') $ BS.drop (succ . (!! 2)
         . BS.elemIndices (c2w '\t') $ s) s

-- | Extract the wave forms from binary data given header information
extractPlot :: Flag       -- ^ Real or Complex Data
             -> Int        -- ^ No. Variables
             -> Int        -- ^ No. Points
             -> ByteString -- ^ Binary Data
             -> [Wave]     -- ^ Wave forms
extractPlot Real'    !numVars !numPoints !bin = seq wave' waves
  where
    -- !wave' = V.generate (numVars * numPoints)
    --        $ \i -> runGet getDoublebe $ BL.drop (fromIntegral i * bytesPerReal) bin
    -- !wave' = V.unsafeCast . V.fromList $ byteSwap' bin :: V.Vector Double
    -- !wave' = castByteStringToVector bin
    (ptr', off, len') = toForeignPtr $ byteSwap bin
    len               = div len' bytesPerReal
    ptr               = castForeignPtr ptr' :: ForeignPtr Double
    !wave'            = V.unsafeFromForeignPtr ptr off len
    !waves            = map RealWave $ r2c numVars numPoints wave'
        -- map RealWave . A.toRows $ A.fromVector (numPoints,numVars) wave'
extractPlot Complex' !numVars !numPoints !bin = seq wave' waves
  where
    -- !wave' = V.generate (numVars * numPoints)
    --       $ \i -> let i'   = fromIntegral $ i * 2
    --                   real = runGet getDoublebe $ BL.drop (i' * bytesPerReal) bin
    --                   imag = runGet getDoublebe $ BL.drop ((i' + 1) * bytesPerReal) bin
    --                in (real:+imag)
    -- !wave' = V.unsafeCast . V.fromList $ byteSwap' bin :: V.Vector (Complex Double)
    -- !wave' = castByteStringToVector bin
    (ptr', off, len') = toForeignPtr $ byteSwap bin
    len               = div len' (2 * bytesPerReal)
    ptr               = castForeignPtr ptr' :: ForeignPtr (Complex Double)
    !wave'            = V.unsafeFromForeignPtr ptr off len
    !waves            = map ComplexWave $ r2c numVars numPoints wave'
        -- map ComplexWave . A.toRows $ A.fromVector (numPoints,numVars) wave'

-- | Read The first plot encountered in ByteString String:
-- @((Plotname, 'Plot'), Remianing ByteString)@
extractPlots :: ByteString -> NutMeg -> NutMeg
extractPlots !bs !nut | BS.isPrefixOf "Plotname:" bs = seq rest . seq plot $ extractPlots rest nut'
                      | otherwise                    = nut
  where
    hdr  = takeWhile (not . BS.isPrefixOf "Binary:") $ CS.lines bs
    ((plotName, flag, numVars, numPoints), varNames) = parseHeader hdr
    n    = (+8) . BS.length $ CS.unlines hdr 
    b    = if flag == Real' then bytesPerReal else 2 * bytesPerReal
    n'   = b * numVars * numPoints
    bin  = BS.take n' $ BS.drop n bs
    plot = M.fromList . zip varNames $! extractPlot flag numVars numPoints bin
    rest = BS.drop (n + n') bs
    nut' = (plotName, plot) : nut

-- | Read a binary nutmeg .raw file
readFile :: FilePath -> IO NutMeg
readFile !path = do
    !plots <- flip extractPlots [] . CS.unlines . drop 2 . CS.lines <$!> BS.readFile path
    pure $! plots
