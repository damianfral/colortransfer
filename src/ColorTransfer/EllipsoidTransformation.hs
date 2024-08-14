{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ColorTransfer.EllipsoidTransformation where

import qualified Codec.Picture as P
import qualified Codec.Picture.Types as P
import ColorTransfer.Utils
import qualified Numeric.LinearAlgebra as L
import Relude hiding (fromList, toList, (<>))

-- Calculate covariance matrix of the pixels
covarianceMatrix :: P.Image P.PixelYCbCr8 -> L.Vector Double -> L.Matrix Double
covarianceMatrix img meanVec = L.scale (1 / fromIntegral (size - 1)) $ P.pixelFold fn zero img
  where
    size = P.imageWidth img * P.imageHeight img
    zero = L.fromLists (replicate 3 (replicate 3 0.0))
    fn !acc _ _ p = acc + L.outer centered centered
      where
        centered = pixelToVector p - meanVec

-- Perform color transfer using ellipsoid method
transferColor :: P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8
transferColor from to =
  let srcMean = meanPixels from
      tarMean = meanPixels to
      srcCov = covarianceMatrix from srcMean
      tarCov = covarianceMatrix to tarMean
      srcCovSqrt = matrixSqrt srcCov
      tarCovSqrt = matrixSqrt tarCov
      srcCovSqrtInv = L.inv srcCovSqrt
   in P.pixelMap (applyTransformation srcMean srcCovSqrtInv tarMean tarCovSqrt) from

-- Apply the transformation matrix to the pixel
applyTransformation ::
  L.Vector Double ->
  L.Matrix Double ->
  L.Vector Double ->
  L.Matrix Double ->
  P.PixelYCbCr8 ->
  P.PixelYCbCr8
applyTransformation !srcMean !srcCovSqrtInv !tarMean !tarCovSqrt (P.PixelYCbCr8 y cb cr) =
  let pixelVec = pixelToVector (P.PixelYCbCr8 y cb cr)
      centered = pixelVec - srcMean
      transformedVec = tarMean + (tarCovSqrt L.#> (srcCovSqrtInv L.#> centered))
      [!y', !cb', !cr'] = clampToWord8 <$> L.toList transformedVec
   in P.PixelYCbCr8 y' cb' cr'

-- Calculate the matrix square root
matrixSqrt :: L.Matrix Double -> L.Matrix Double
matrixSqrt m =
  let (eigVals, eigVecs) = L.eigSH $ L.sym m
      sqrtEigVals = L.diag $ L.cmap sqrt eigVals
   in eigVecs L.<> sqrtEigVals L.<> L.tr eigVecs
