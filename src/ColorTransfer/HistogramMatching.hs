{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ColorTransfer.HistogramMatching where

import qualified Codec.Picture as P
import ColorTransfer.Utils
import qualified Numeric.LinearAlgebra as L
import Relude hiding (fromList, toList)

-- Perform color transfer using histogram matching
transferColor :: P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8
transferColor input reference = P.pixelMap (transfer srcMean srcStd tarMean tarStd) input
  where
    !srcMean = meanPixels input
    !srcStd = stdPixels input
    !tarMean = meanPixels reference
    !tarStd = stdPixels reference

-- Apply the transfer function to a single pixel
transfer ::
  L.Vector Double -> -- srcMean
  L.Vector Double -> -- srcStd
  L.Vector Double -> -- tarMean
  L.Vector Double -> -- tarStd
  P.PixelYCbCr8 ->
  P.PixelYCbCr8
transfer srcMean srcStd tarMean tarStd pixel =
  let pixelVec = pixelToVector pixel
      transformed = (tarStd / srcStd) * (pixelVec - tarMean) + srcMean
      [y', cb', cr'] = map clampToWord8 (L.toList transformed)
   in P.PixelYCbCr8 y' cb' cr'
