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
import ColorTransfer.EllipsoidTransformation qualified as ET
import ColorTransfer.HistogramMatching qualified as HM
import Control.Applicative
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Either
import Data.Word
import GHC.Generics (Generic)
import Options.Generic
import System.Environment

data Opts w = Opts
  { source :: w ::: String <?> "the image to be used as the color source",
    target :: w ::: String <?> "the image we want to transform",
    output :: w ::: String <?> "the output image",
    histogram :: w ::: Bool <?> "use simple histogram matching"
  }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

run :: Opts Unwrapped -> IO ()
run (Opts {..}) = do
  sourceImg <- P.readImage source
  targetImg <- P.readImage target

  let result =
        (if histogram then HM.transferColor else ET.transferColor)
          <$> (HM.dynamicImagetoYCbCr <$> sourceImg)
          <*> (HM.dynamicImagetoYCbCr <$> targetImg)

  case result of
    Right img -> P.writePng output $ HM.imageToRGB img
    Left err -> putStrLn err

main :: IO ()
main = do
  options <- unwrapRecord "Transfer color between images"
  run options
