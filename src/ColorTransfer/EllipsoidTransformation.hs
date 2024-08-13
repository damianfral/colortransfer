{-# LANGUAGE NoImplicitPrelude #-}

module ColorTransfer.EllipsoidTransformation where

import qualified Codec.Picture as P
import qualified Codec.Picture.Types as P
import qualified Numeric.LinearAlgebra as L
import Relude hiding (fromList, toList, (<>))

type Triplet a = (a, a, a)

-- Convert PixelYCbCr8 to Vector Double
pixelToVector :: P.PixelYCbCr8 -> L.Vector Double
pixelToVector (P.PixelYCbCr8 y cb cr) =
  L.vector $ fromIntegral <$> [y, cb, cr]

-- Calculate mean vector of the pixels
meanPixels :: P.Image P.PixelYCbCr8 -> L.Vector Double
meanPixels img =
  (/ fromIntegral size) $ P.pixelFold (\acc _ _ p -> acc + pixelToVector p) zero img
  where
    size = P.imageWidth img * P.imageHeight img
    zero = L.vector [0.0, 0.0, 0.0]

-- Calculate covariance matrix of the pixels
covarianceMatrix :: P.Image P.PixelYCbCr8 -> L.Vector Double -> L.Matrix Double
covarianceMatrix img meanVec =
  (/ fromIntegral (size - 1)) $ P.pixelFold fn zero img
  where
    size = P.imageWidth img * P.imageHeight img
    zero =
      L.fromRows
        [ L.vector [0.0, 0.0, 0.0],
          L.vector [0.0, 0.0, 0.0],
          L.vector [0.0, 0.0, 0.0]
        ]
    fn acc _ _ p = acc + L.outer centered centered
      where
        centered = pixelToVector p - meanVec

-- Perform color transfer using ellipsoid method
transferColor ::
  P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8
transferColor from to =
  P.pixelMap (applyTransformation srcMean srcCov tarMean tarCov) from
  where
    srcMean = meanPixels from
    tarMean = meanPixels to
    srcCov = covarianceMatrix from srcMean
    tarCov = covarianceMatrix to tarMean

-- Apply the transformation matrix to the pixel
applyTransformation ::
  L.Vector Double ->
  L.Matrix Double ->
  L.Vector Double ->
  L.Matrix Double ->
  P.PixelYCbCr8 ->
  P.PixelYCbCr8
applyTransformation srcMean srcCov tarMean tarCov (P.PixelYCbCr8 y cb cr) =
  let pixelVec = pixelToVector (P.PixelYCbCr8 y cb cr)
      centered = pixelVec - srcMean
      srcCovSqrt = matrixSqrt srcCov
      tarCovSqrt = matrixSqrt tarCov
      srcCovSqrtInv = L.inv srcCovSqrt
      transformedVec = tarMean + (tarCovSqrt L.#> (srcCovSqrtInv L.#> centered))
      [y', cb', cr'] = clampToWord8 <$> L.toList transformedVec
   in P.PixelYCbCr8 y' cb' cr'

-- Calculate the matrix square root
matrixSqrt :: L.Matrix Double -> L.Matrix Double
matrixSqrt m =
  let (eigVals, eigVecs) = L.eigSH $ L.sym m
      sqrtEigVals = L.diag $ L.cmap sqrt eigVals
   in eigVecs L.<> sqrtEigVals L.<> L.tr eigVecs

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
