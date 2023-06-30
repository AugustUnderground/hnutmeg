{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           Data.NutMeg

import           Control.DeepSeq
import           Control.Scheduler
import           System.Clock
import qualified Data.Binary.Get             as B
import           Data.Function                       (on)
import           Data.Maybe                          (fromJust, mapMaybe)
import qualified Data.Map                    as M
import qualified Data.List                   as L
import qualified Data.Vector.Unboxed         as V
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Char8  as CL
import qualified Data.Text                   as T
import           Graphics.Vega.VegaLite             hiding (sample, shape)

tranTest :: IO ()
tranTest = do
    !nut <- readNutRaw' "./example/nutbin.raw"
    -- !nut <- parseNutMeg <$> readNutRaw "./example/nutbin.raw"

    let tran = "Transient Analysis `tran': time = (0 s -> 5 ns)"
        plt  = snd . head . L.filter ( (== tran) . fst ) $ nutPlots nut
        vm   = M.map asRealVector . nutData $ plt

        xParam = "time"
        yParam = "O"
        
        t =  V.toList $ fromJust (M.lookup xParam vm)
        o =  V.toList $ fromJust (M.lookup yParam vm)

    plotNut "./example/nutbin.html" "time (s)" "O(V)" (zip t o)

nmosTest :: IO ()
nmosTest  = do

    !tic <- getTime Realtime
    !nut <- readNutRaw' "./example/nutmos.raw"
    !toc <- getTime Realtime
    let !td = (*1.0e-9) . realToFrac . toNanoSecs $ diffTimeSpec toc tic :: Float
    putStrLn $ "1x : " ++ show td ++ "s"

    let n = 5
    !tic' <- getTime Realtime
    -- !nut' <- traverseConcurrently Par' (fmap parseNutMeg . readNutRaw)
    --             $ replicate n "./example/nutmos.raw"
    !nut' <- replicateConcurrently Par' n (readNutRaw' "./example/nutmos.raw")
    !toc' <- getTime Realtime
    let !td' = (*1.0e-9) . realToFrac . toNanoSecs $ diffTimeSpec toc' tic' :: Float
    putStrLn $ show n  ++ "x : " ++ show td' ++ "s"

    let plt    = flattenRealPlots . map snd . nutPlots $ nut
        vm     = M.map asRealVector . nutData $ plt
        
        xParam = "M0:vgs"
        yParam = "M0:id"
        td = traceData vm xParam yParam

    putStrLn $ "Plotting " ++ (show . length $ td ) ++ " data points."
    plotNut "./example/nutmos.html" "Vgs (V)" "Id (A)" td 

main :: IO ()
main = do
    putStrLn "Transient test (./examples/nutbin.raw)."
    tranTest
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
               . L.transpose . map (V.toList . fromJust . (`M.lookup` m)) $ p
    where p = M.keys m
