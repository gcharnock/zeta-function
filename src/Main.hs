
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import Data.Complex

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

chart :: Renderable ()
chart = toRenderable layout
  where
    sumf = plot_lines_values .~ [[ (x,(s x)) | x <- [1.3,(1.31)..10]]]
              $ plot_lines_style  . line_color .~ opaque black
              $ plot_lines_title .~ "S(a)"
              $ def

    tln = plot_lines_values .~ [[ (x,(taylor1 2.0 x)) | x <- [1.3,(1.4)..3.5]]]
              $ plot_lines_style  . line_color .~ opaque red
              $ plot_lines_title .~ "First taylor"
              $ def

    tln2 = plot_lines_values .~ [[ (x,(taylor2 2.0 x)) | x <- [1.3,(1.4)..4.0]]]
              $ plot_lines_style  . line_color .~ opaque orange
              $ plot_lines_title .~ "Second taylor"
              $ def

    tln3 = plot_lines_values .~ [[ (x,(taylor3 2.0 x)) | x <- [1.3,(1.4)..3.2]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "Third taylor"
              $ def

    layout = layout_title .~ "Amplitude Modulation"
           $ layout_plots .~ [toPlot sumf, toPlot tln, toPlot tln2, toPlot tln3]
           $ def

renderChart :: IO (PickFn ())
renderChart = renderableToFile def "example1_big.png" chart

approxS :: Int -> Double -> Double
approxS i a = sum $ map (f.fromIntegral) [1..i]
  where f n = n**(-a)

class Abs a where
  absoluteValue :: a -> Double

instance Abs Double where
  absoluteValue = abs

instance Abs (Complex Double) where
  absoluteValue = magnitude

untilConverged :: (Abs a, Floating a, Num a) => Double -> [a] -> a
untilConverged epsilon (x1:xs) = go x1 xs
  where go last (y:ys) =
         if absoluteValue (last - y) < epsilon
           then y
           else go y ys

sumSeries :: (Abs a, Floating a, Num a) => (Int -> a) -> a
sumSeries seriesGen = untilConverged 0.000001 $ tail $ scanl1 (+) $ map seriesGen [1..]

s :: Double -> Double
s a = sumSeries $ \n-> (fromIntegral n)**(-a)

sDerivative :: (Abs a, Floating a, Num a) => Int -> a -> a
sDerivative d a = sign * (sumSeries $ \nInt -> 
    let n = fromIntegral nInt in
    (fromIntegral nInt)**(-a) * (realToFrac $ log n)^d)
  where sign = if even d then 1 else (-1)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

taylorCoeffients :: (Fractional a, Num a) => (Int -> a -> a) -> a -> [a]
taylorCoeffients f a0 = map g [0..]
  where g n = (f n a0) / fromIntegral (fact n)

taylor1 :: Double -> Double -> Double
taylor1 a0 a = s a0 + (a - a0) * sDerivative 1 a0

taylor2 :: Double -> Double -> Double
taylor2 a0 a = s a0 + (a - a0) * sDerivative 1 a0 + 0.5 * (a - a0)^2 * sDerivative 2 a0

taylor3 :: Double -> Double -> Double
taylor3 a0 a = s a0 + (a - a0) * sDerivative 1 a0 + 0.5 * (a - a0)^2 * sDerivative 2 a0 + (1.0/6.0) * (a - a0)^3 * sDerivative 3 a0

main :: IO ()
main = void $ renderChart


