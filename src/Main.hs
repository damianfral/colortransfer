{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Codec.Picture qualified as P
import Codec.Picture.Saving qualified as P
import Codec.Picture.Types
import Codec.Picture.Types qualified as P
import Control.Applicative
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Either
import Data.Word
import GHC.Generics (Generic)
import Options.Generic
import System.Environment

type Triplet a = (a, a, a)

sumPixels :: P.Image P.PixelYCbCr8 -> Triplet Double
sumPixels = P.pixelFold fn (0.0, 0.0, 0.0)
  where
    fn :: Triplet Double -> Int -> Int -> P.PixelYCbCr8 -> Triplet Double
    fn (a', b', c') _ _ ((P.PixelYCbCr8 a b c)) = (aa, bb, cc)
      where
        f a b = fromIntegral a + b
        !aa = f a a'
        !bb = f b b'
        !cc = f c c'

meanPixels :: P.Image P.PixelYCbCr8 -> Triplet Double
meanPixels img = fn $ sumPixels img
  where
    size = fromIntegral $ P.imageWidth img * P.imageHeight img
    fn (x, y, z) = (x / size, y / size, z / size)

stdPixels :: P.Image P.PixelYCbCr8 -> Triplet Double
stdPixels img = g $ P.pixelFold fn (0.0, 0.0, 0.0) img
  where
    size = fromIntegral $ P.imageWidth img * P.imageHeight img
    (ma, mb, mc) = meanPixels img
    g (a, b, c) = (sqrt (a / size), sqrt (b / size), sqrt (c / size))
    fn ::
      Triplet Double ->
      Int ->
      Int ->
      P.PixelYCbCr8 ->
      Triplet Double
    fn (a', b', c') _ _ ((P.PixelYCbCr8 a b c)) = (aa, bb, cc)
      where
        f a b = (fromIntegral a - b) ** 2
        !aa = a' + f a ma
        !bb = b' + f b mb
        !cc = c' + f c mc

transferColor :: P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8
transferColor from to = P.pixelMap (transfer srcMean srcStd tarMean tarStd) to
  where
    (!srcMean, !srcStd) = (meanPixels from, stdPixels from)
    (!tarMean, !tarStd) = (meanPixels to, stdPixels to)

{-# INLINE zip5Triplets #-}
zip5Triplets ::
  (a -> a -> a -> a -> a -> b) ->
  (a, a, a) ->
  (a, a, a) ->
  (a, a, a) ->
  (a, a, a) ->
  (a, a, a) ->
  (b, b, b)
zip5Triplets fn (a, b, c) (d, e, f) (g, h, i) (j, k, l) (m, n, o) =
  (fn a d g j m, fn b e h k n, fn c f i l o)

{-# INLINE clampToWord8 #-}
clampToWord8 :: Double -> Word8
clampToWord8 a
  | a > 255 = 255
  | a < 0 = 0
  | otherwise = round a

transfer ::
  Triplet Double ->
  Triplet Double ->
  Triplet Double ->
  Triplet Double ->
  P.PixelYCbCr8 ->
  P.PixelYCbCr8
transfer srcMean srcStd tarMean tarStd (P.PixelYCbCr8 a b c) =
  P.PixelYCbCr8 a' b' c'
  where
    f :: Double -> Double -> Double -> Double -> Double -> Word8
    f m s m' s' p = clampToWord8 (s' / s * (p - m') + m)
    tmp = (fromIntegral a, fromIntegral b, fromIntegral c) :: Triplet Double
    (a', b', c') = zip5Triplets f srcMean srcStd tarMean tarStd tmp

dynamicImagetoYCbCr :: P.DynamicImage -> P.Image P.PixelYCbCr8
dynamicImagetoYCbCr (P.ImageRGB8 img) = P.convertImage img
dynamicImagetoYCbCr (P.ImageYCbCr8 img) = P.promoteImage img

imageToRGB :: P.Image P.PixelYCbCr8 -> P.Image P.PixelRGB8
imageToRGB = P.convertImage

data Opts w = Opts
  { source :: w ::: String <?> "the image to be used as the color source",
    target :: w ::: String <?> "the image we want to transform",
    output :: w ::: String <?> "the output image"
  }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

run :: Opts Unwrapped -> IO ()
run (Opts {..}) = do
  sourceImg <- P.readImage source
  targetImg <- P.readImage target

  let result =
        transferColor
          <$> (dynamicImagetoYCbCr <$> sourceImg)
          <*> (dynamicImagetoYCbCr <$> targetImg)

  case result of
    Right img -> P.writePng output $ imageToRGB img
    Left err -> putStrLn err

main :: IO ()
main = do
  options <- unwrapRecord "Transfer color between images"
  run options
