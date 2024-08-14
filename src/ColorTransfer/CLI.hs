{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ColorTransfer.CLI where

import qualified Codec.Picture as P
import qualified ColorTransfer.EllipsoidTransformation as ET
import qualified ColorTransfer.HistogramMatching as HM
import ColorTransfer.Utils
import Options.Generic
import Relude

data Opts w = Opts
  { input :: w ::: String <?> "the image to be used as the color source",
    reference :: w ::: String <?> "the image we want to transform",
    output :: w ::: String <?> "the output image",
    method :: w ::: Int <!> "2" <?> "1 histogram matching, 2 ellipsoid transformation"
  }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

runColorTransfer :: Opts Unwrapped -> IO ()
runColorTransfer (Opts {..}) = do
  inputImg <- P.readImage input
  referenceImg <- P.readImage reference
  let transfer = if method == 1 then HM.transferColor else ET.transferColor
  let result =
        transfer
          <$> (dynamicImagetoYCbCr <$> inputImg)
          <*> (dynamicImagetoYCbCr <$> referenceImg)

  case result of
    Right img -> writeFileLBS output $ P.encodeJpeg img
    Left err -> putStrLn err
