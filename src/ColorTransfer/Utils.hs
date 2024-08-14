module ColorTransfer.Utils where

import qualified Codec.Picture as P
import qualified Codec.Picture.Types as P
import Data.Word (Word8)
import qualified Numeric.LinearAlgebra as L

-- Convert dynamic image to YCbCr8 image
dynamicImagetoYCbCr :: P.DynamicImage -> P.Image P.PixelYCbCr8
dynamicImagetoYCbCr (P.ImageRGB8 img) = P.convertImage img
dynamicImagetoYCbCr (P.ImageYCbCr8 img) = P.promoteImage img
dynamicImagetoYCbCr _ = undefined

-- Convert image back to RGB8
imageToRGB :: P.Image P.PixelYCbCr8 -> P.Image P.PixelRGB8
imageToRGB = P.convertImage

{-# INLINE clampToWord8 #-}
clampToWord8 :: Double -> Word8
clampToWord8 a
  | a > 255 = 255
  | a < 0 = 0
  | otherwise = round a

-- Convert PixelYCbCr8 to Vector Double
pixelToVector :: P.PixelYCbCr8 -> L.Vector Double
pixelToVector (P.PixelYCbCr8 y cb cr) =
  L.vector $ fromIntegral <$> [y, cb, cr]

-- Calculate mean vector of the pixels
meanPixels :: P.Image P.PixelYCbCr8 -> L.Vector Double
meanPixels img =
  P.pixelFold sumColorChannels zero img / fromIntegral size
  where
    sumColorChannels acc _ _ p = acc + pixelToVector p
    size = P.imageWidth img * P.imageHeight img
    zero = L.vector [0.0, 0.0, 0.0]

-- Calculate standard deviation vector of the pixels
stdPixels :: P.Image P.PixelYCbCr8 -> L.Vector Double
stdPixels img = L.cmap sqrt $ (/ fromIntegral (size - 1)) $ P.pixelFold fn (L.vector [0, 0, 0]) img
  where
    size = P.imageWidth img * P.imageHeight img
    mean = meanPixels img
    fn acc _ _ p = acc + (centered * centered)
      where
        centered = pixelToVector p - mean
