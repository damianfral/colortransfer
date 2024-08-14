{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import System.IO (hPutStrLn)

data Opts w = Opts
  { input :: w ::: String <?> "the image to be used as the color source",
    reference :: w ::: String <?> "the image we want to transform",
    output :: w ::: String <?> "the output image",
    method :: w ::: Int <!> "2" <?> "1 histogram matching, 2 ellipsoid transformation"
  }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

runColorTransfer :: Int -> FilePath -> FilePath -> IO (Either String LByteString)
runColorTransfer method input reference = do
  inputImg <- P.readImage input
  referenceImg <- P.readImage reference
  let transfer = if method == 1 then HM.transferColor else ET.transferColor
  let result =
        transfer
          <$> (dynamicImagetoYCbCr <$> inputImg)
          <*> (dynamicImagetoYCbCr <$> referenceImg)
  pure $ P.encodeJpeg <$> result

runColorTransferAndExit :: Opts Unwrapped -> IO ()
runColorTransferAndExit Opts {..} =
  runColorTransfer method input reference >>= \case
    Right jpeg -> writeFileLBS output jpeg >> exitSuccess
    Left err -> hPutStrLn stderr err >> exitFailure
