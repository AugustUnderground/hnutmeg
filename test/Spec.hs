{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.NutMeg

import Data.Function (on)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector.Unboxed as V
import qualified Data.Matrix.Unboxed as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as CS
import qualified Data.Text as T
import Graphics.Vega.VegaLite hiding (sample, shape)

tranTest :: IO ()
tranTest = do
    nut <- parseNutMeg <$> readNutRaw "./example/nutbin.raw"
    
    let nutMap   = nutPlots nut
        Just plt = M.lookup (last $ M.keys nutMap) nutMap
        vm   = M.map asRealVector . nutData $ plt

        xParam = "time"
        yParam = "O"
        
        t =  V.toList $ fromJust (M.lookup xParam vm)
        o =  V.toList $ fromJust (M.lookup yParam vm)

    plotNut "./example/nutbin.html" "time (s)" "O(V)" (zip t o)

nmosTest :: IO ()
nmosTest  = do
    nut <- parseNutMeg <$> readNutRaw "./example/nutmos.raw"
    
    let nutMap = M.fromList [( "DC Analysis" , flattenRealPlots . M.elems . nutPlots $ nut )]
        -- nut'   = NutMeg (nutTitle nut) (nutDate nut) nutMap
        plt    = snd . head . M.toList $ nutMap -- . nutPlots $ nut'
        vm     = M.map asRealVector . nutData $ plt
        
        xParam = "M0:vgs"
        yParam = "M0:id"
        td = traceData vm xParam yParam

    putStrLn $ "Plotting " ++ (show . length $ td ) ++ " data points."
    plotNut "./example/nutmos.html" "Vgs (V)" "Id (A)" td 

main :: IO ()
main = do
    --putStrLn "Transient test (./examples/nutbin.raw)."
    --tranTest
    putStrLn "NMOS test (./examples/nutmos.raw)."
    nmosTest
    putStrLn "All tests done."

plotNut :: FilePath -> T.Text -> T.Text -> [(Double, Double)] -> IO ()
plotNut pf xl yl xy = toHtmlFile pf 
                    $ toVegaLite [ dt []
                                 , mark Line []
                                 , enc []
                                 , height 800
                                 , width 800 ]
    where axis = PAxis [ AxValues (Numbers (map fst xy)) ]
          enc  = encoding . position X [ PName xl, PmType Quantitative, axis ]
                          . position Y [ PName yl, PmType Quantitative ]
                          . color [ MName "Lines", MmType Nominal ]
          dt   = foldl (\sum' (x, y) ->
                            sum' . dataRow [ (xl, Number x) 
                                           , (yl, Number y) ]
                       ) (dataFromRows []) xy

traceData :: M.Map String (V.Vector Double) -> String -> String 
          -> [(Double, Double)]
traceData dat xParam yParam = zip x y
    where 
      params   = M.keys dat
      Just vds = L.elemIndex "M0:vds" params
      Just vgs = L.elemIndex "M0:vgs" params
      Just vbs = L.elemIndex "M0:vbs" params
      Just l   = L.elemIndex "L" params
      Just ll  = (!!1) . L.nub . V.toList <$> M.lookup "L" dat
      Just w   = L.elemIndex "W" params
      Just ww  = (!!1) . L.nub . V.toList <$> M.lookup "W" dat
      fd       = \d -> (roundn (d !! vds) 2 == 0.6) 
                        && ((d !! l) == ll) 
                        && ((d !! w) == ww) 
                        && (roundn (d !! vbs) 2 == 0.00)
      Just xp   = L.elemIndex xParam params
      !traceData = sortData xp . filterData fd $ dat
      Just x = M.lookup xParam traceData
      Just y = M.lookup yParam traceData

sortData :: Int -> M.Map String [Double] -> M.Map String [Double]
sortData n m = M.fromList . zip p . L.transpose . L.sortBy f 
             . L.transpose . mapMaybe (`M.lookup` m) $ p
    where p = M.keys m
          f = compare `on` (!! n)

roundn :: Double -> Int -> Double
roundn d n = fromInteger (round $ d * (10^n)) / (10.0^^n)

filterData :: ([Double] -> Bool) -> M.Map String (V.Vector Double) -> M.Map String [Double]
filterData f m = M.fromList . zip p . L.transpose . L.filter f
               . map V.toList . A.toRows . A.fromColumns 
               . mapMaybe (`M.lookup` m) $ p
    where p = M.keys m
