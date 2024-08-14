{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module ColorTransfer.CLI where

import qualified Codec.Picture as P
import qualified ColorTransfer.EllipsoidTransformation as ET
import qualified ColorTransfer.HistogramMatching as HM
import ColorTransfer.Utils
import Options.Generic

data Opts w = Opts
  { input :: w ::: String <?> "the image to be used as the color source",
    reference :: w ::: String <?> "the image we want to transform",
    output :: w ::: String <?> "the output image",
    histogram :: w ::: Bool <?> "use simple histogram matching"
  }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

runColorTransfer :: Opts Unwrapped -> IO ()
runColorTransfer (Opts {..}) = do
  inputImg <- P.readImage input
  referenceImg <- P.readImage reference
  let result =
        (if histogram then HM.transferColor else ET.transferColor)
          <$> (dynamicImagetoYCbCr <$> inputImg)
          <*> (dynamicImagetoYCbCr <$> referenceImg)

  case result of
    Right img -> P.writePng output $ imageToRGB img
    Left err -> putStrLn err
