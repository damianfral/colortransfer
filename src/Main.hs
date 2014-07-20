{-# OPTIONS_GHC -funbox-strict-fields -threaded -O2 #-}
{-# LANGUAGE BangPatterns #-}


module Main where
import Codec.Picture.Types
import Control.Applicative
import Data.Either
import Data.Word
import Options.Applicative
import System.Environment

import qualified Codec.Picture as P
import qualified Codec.Picture.Saving as P
import qualified Codec.Picture.Types as P
import qualified Data.ByteString.Lazy.Char8 as B


sumPixels :: P.Image P.PixelYCbCr8 -> (Double,Double,Double)
sumPixels = P.pixelFold fn (0.0,0.0,0.0)
    where
        fn :: (Double, Double, Double) -> Int -> Int -> P.PixelYCbCr8 -> (Double, Double, Double)
        fn (a', b', c') _ _ ((P.PixelYCbCr8 a b c)) = (aa, bb, cc)
            where
                f a b = fromIntegral a + b
                !aa   = f a a'
                !bb   = f b b'
                !cc   = f c c'

meanPixels :: P.Image P.PixelYCbCr8 -> (Double,Double,Double)
meanPixels img = fn $ sumPixels img
    where
        size       = fromIntegral $ P.imageWidth img * (P.imageHeight img)
        fn (x,y,z) = (x / size, y / size, z / size)

stdPixels :: P.Image P.PixelYCbCr8 -> (Double,Double,Double)
stdPixels img = g $ P.pixelFold fn (0.0,0.0,0.0) img
    where
        size         = fromIntegral $ P.imageWidth img * (P.imageHeight img)
        (ma, mb, mc) =  meanPixels img
        g (a,b,c)    = (sqrt (a / size) , sqrt (b / size) , sqrt  (c / size))
        fn :: (Double, Double, Double) -> Int -> Int -> P.PixelYCbCr8 -> (Double, Double, Double)
        fn (a', b', c') _ _ ((P.PixelYCbCr8 a b c)) = (aa, bb, cc)
            where
                f a b = ((fromIntegral a) - b) ** 2
                !aa = a' + (f a ma)
                !bb = b' + (f b mb)
                !cc = c' + (f c mc)

transferColor :: P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8
transferColor from to = P.pixelMap (transfer  srcMean srcStd tarMean tarStd) to
    where
        (!srcMean, !srcStd) = (meanPixels from, stdPixels from)
        (!tarMean, !tarStd) = (meanPixels to, stdPixels to)

{-# INLINE zip5Triplets #-}
zip5Triplets :: (a -> a -> a -> a -> a -> b) -> (a,a,a) -> (a,a,a) ->  (a,a,a) -> (a,a,a) ->  (a,a,a) -> (b,b,b)
zip5Triplets fn (a,b,c) (d,e,f) (g,h,i) (j,k,l) (m,n,o) = ((fn a d g j m), (fn b e h k n), (fn c f i l o))

{-# INLINE clampToWord8 #-}
clampToWord8 ::  Double -> Word8
clampToWord8 a | a > 255 = 255
               | a < 0 = 0
               | otherwise = round a

transfer  :: (Double,Double,Double) ->  (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double) -> P.PixelYCbCr8 -> P.PixelYCbCr8
transfer  srcMean srcStd tarMean tarStd (P.PixelYCbCr8 a b c) = (P.PixelYCbCr8 a' b' c')
    where
        f :: Double -> Double -> Double -> Double -> Double -> Word8
        f m s m' s' p = clampToWord8  ((s' / s  * (p - m')) + m)
        tmp           = (fromIntegral a, fromIntegral b, fromIntegral c)::(Double,Double,Double)
        (a',b',c')    = (zip5Triplets f srcMean srcStd tarMean tarStd tmp)

dynamicImagetoYCbCr ::  P.DynamicImage -> P.Image P.PixelYCbCr8
dynamicImagetoYCbCr (P.ImageRGB8   img) = P.convertImage img
dynamicImagetoYCbCr (P.ImageYCbCr8 img) = P.promoteImage img

imageToRGB ::  P.Image P.PixelYCbCr8 -> P.Image P.PixelRGB8
imageToRGB img = P.convertImage img

data Opts = Opts
    { _source::String
    , _target::String
    , _output::String
    }
opts :: Parser Opts
opts = Opts <$> sourceFile <*> targetFile <*> outputFile
    where
        sourceFile = strOption (long "source" <> metavar "FILENAME" <> help "Source filename" )
        targetFile = strOption (long "target" <> metavar "FILENAME" <> help "Target filename")
        outputFile = strOption (long "output" <> metavar "FILENAME" <> help "Source filename")

run :: Opts -> IO ()
run (Opts source target output) = do
    sourceImg <- P.readImage source
    targetImg <- P.readImage target

    case transferColor <$> (dynamicImagetoYCbCr <$> sourceImg) <*> (dynamicImagetoYCbCr <$> targetImg) of
        Right (img) -> P.writePng output $ imageToRGB img
        Left err    -> putStrLn err

main :: IO ()
main = execParser options >>= run
 where
    options = info
              (helper <*> opts)
              ( fullDesc <> progDesc "Transfer color between images")
