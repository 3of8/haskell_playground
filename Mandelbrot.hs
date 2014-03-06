{-
    Compile with -threaded, run with +RTS -N<i> -RTS, where <i> is the number of threads to use.
-}

module Main where

import Data.Complex
import qualified Data.Vector as V
import Data.Vector ((!))
import Codec.Picture.Types
import Codec.Picture.Png
import Control.Parallel.Strategies

import GHC.Conc (numCapabilities)
import System.CPUTime
import Data.Time
import Text.Printf
import System.Environment (getArgs)
import System.IO


colors :: RealFrac a => V.Vector (a, a, a)
colors = V.fromList
            [(0.00000, 0.00000, 0.50000),(0.00000, 0.00000, 0.56349),(0.00000, 0.00000, 0.62698),
              (0.00000, 0.00000, 0.69048),(0.00000, 0.00000, 0.75397),(0.00000, 0.00000, 0.81746),
              (0.00000, 0.00000, 0.88095),(0.00000, 0.00000, 0.94444),(0.00000, 0.00794, 1.00000),
              (0.00000, 0.07143, 1.00000),(0.00000, 0.13492, 1.00000),(0.00000, 0.19841, 1.00000),
              (0.00000, 0.26190, 1.00000),(0.00000, 0.32540, 1.00000),(0.00000, 0.38889, 1.00000),
              (0.00000, 0.45238, 1.00000),(0.00000, 0.51587, 1.00000),(0.00000, 0.57937, 1.00000),
              (0.00000, 0.64286, 1.00000),(0.00000, 0.70635, 1.00000),(0.00000, 0.76984, 1.00000),
              (0.00000, 0.83333, 1.00000),(0.00000, 0.89683, 1.00000),(0.00000, 0.96032, 1.00000),
              (0.02381, 1.00000, 0.97619),(0.08730, 1.00000, 0.91270),(0.15079, 1.00000, 0.84921),
              (0.21429, 1.00000, 0.78571),(0.27778, 1.00000, 0.72222),(0.34127, 1.00000, 0.65873),
              (0.40476, 1.00000, 0.59524),(0.46825, 1.00000, 0.53175),(0.53175, 1.00000, 0.46825),
              (0.59524, 1.00000, 0.40476),(0.65873, 1.00000, 0.34127),(0.72222, 1.00000, 0.27778),
              (0.78571, 1.00000, 0.21429),(0.84921, 1.00000, 0.15079),(0.91270, 1.00000, 0.08730),
              (0.97619, 1.00000, 0.02381),(1.00000, 0.96032, 0.00000),(1.00000, 0.89683, 0.00000),
              (1.00000, 0.83333, 0.00000),(1.00000, 0.76984, 0.00000),(1.00000, 0.70635, 0.00000),
              (1.00000, 0.64286, 0.00000),(1.00000, 0.57937, 0.00000),(1.00000, 0.51587, 0.00000),
              (1.00000, 0.45238, 0.00000),(1.00000, 0.38889, 0.00000),(1.00000, 0.32540, 0.00000),
              (1.00000, 0.26190, 0.00000),(1.00000, 0.19841, 0.00000),(1.00000, 0.13492, 0.00000),
              (1.00000, 0.07143, 0.00000),(1.00000, 0.00794, 0.00000),(0.94444, 0.00000, 0.00000),
              (0.88095, 0.00000, 0.00000),(0.81746, 0.00000, 0.00000),(0.75397, 0.00000, 0.00000),
              (0.69048, 0.00000, 0.00000),(0.62698, 0.00000, 0.00000),(0.56349, 0.00000, 0.00000),
              (0.50000, 0.00000, 0.00000)]

colorToPixel :: RealFrac a => (a,a,a) -> PixelRGB8
colorToPixel (r,g,b) = PixelRGB8 (doubleToWord8 r) (doubleToWord8 g) (doubleToWord8 b)
  where doubleToWord8 x = max 0 $ min 255 $ round $ 255 * x

mixColor :: RealFrac a => a -> (a,a,a) -> (a,a,a) -> (a,a,a)
mixColor f (r1,g1,b1) (r2,g2,b2) = (r1*f+r2*(1-f), g1*f+g2*(1-f), b1*f+b2*(1-f))


newtype ColorMap a = ColorMap (a -> PixelRGB8)

rangedColorMap :: RealFrac a => a -> a -> ColorMap a
rangedColorMap lo hi = 
    ColorMap $ \x -> let di = fromIntegral (V.length colors - 1) * (x - lo) / (hi - lo)
                         (i, f) = properFraction di
                     in  colorToPixel $
                           if i < 0 then V.head colors
                           else if i >= V.length colors - 1 then V.last colors
                           else mixColor f (colors ! i) (colors ! (i+1))


linspace :: (Fractional a, Integral b) => a -> a -> b -> V.Vector a
linspace l r n = V.fromList [l  + fromIntegral k * (r - l) / (fromIntegral n - 1) | k <- [0..n-1]]

mandel :: (RealFloat a, Num b) => a -> Int -> Complex a -> b
mandel b n x = case is of {[] -> fromIntegral (n + 1); (m:_) -> fromIntegral m}
    where is = [i | (i,x) <- zip [0..n] (iterate (\y -> y^2 + x) x), magnitude x > b] :: [Int]
    
mandel' :: (RealFloat a, Num b) => a -> Int -> a -> a -> b
mandel' b n x y = mandel b n (x :+ y)

plot :: RealFrac a => ColorMap a -> (a -> a -> a) -> V.Vector a -> V.Vector a -> Image PixelRGB8
plot (ColorMap m) f xs ys = generateImage f' w h
  where (w, h) = (V.length xs, V.length ys)
        f' i j = m (f (xs ! i) (ys ! (h - j - 1)))
        

plotDistributed :: RealFrac a => Int -> ColorMap a -> (a -> a -> a) -> V.Vector a -> V.Vector a -> Image PixelRGB8
plotDistributed n m f xs ys = 
  let parts = map (\xs -> plot m f xs ys) $ chunks ((V.length xs + n - 1) `div` n) xs
      parts' = parts `using` parList rdeepseq
  in  mergeImages [parts']
  where chunks k v = if V.length v <= k then [v] else let (v', vs') = V.splitAt k v in v' : chunks k vs'

mergeImages :: Pixel a => [[Image a]] -> Image a
mergeImages imgs = generateImage f w h
  where (w, h) = (sum (map imageWidth (head imgs)), sum (map (imageHeight . head) imgs))
        (iw, ih) = (imageWidth (head (head imgs)), imageHeight (head (head imgs)))
        imgs' = V.fromList (map V.fromList imgs)
        f x y =
          let ((i, iy), (j, ix)) = (y `divMod` ih, x `divMod` iw)
          in  pixelAt (imgs' ! i ! j) ix iy

main =
  do hSetBuffering stdout NoBuffering
     args <- getArgs
     let (w, h) = case args of 
                    (sw:sh:_) -> (read sw, read sh)
                    _ -> (1920 :: Int, 1080 :: Int)
     let r = fromIntegral w / fromIntegral h * 3 / 4 :: Double
     let xs = linspace (-2 * r) r w
     let ys = linspace (-1) 1 h
     putStr $ printf "Rendering a %d×%d image with %d threads... " w h numCapabilities
     t1 <- getCurrentTime
     ct1 <- getCPUTime
     let img = plotDistributed (10*numCapabilities) (rangedColorMap 0 201) (mandel' 10 200) xs ys
     writePng "mandelbrot.png" img
     t2 <- getCurrentTime
     ct2 <- getCPUTime
     let dt = diffUTCTime t2 t1
     putStrLn $ printf "Done.\n%.1f seconds elapsed (%.1f seconds CPU time)" 
            (fromRational (toRational dt) :: Double) (fromIntegral (ct2 - ct1) * 1e-12 :: Double) 



