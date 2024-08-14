{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified ColorTransfer.CLI as CLI
import Options.Generic

main :: IO ()
main = do
  options <- unwrapRecord "Transfer color between images"
  CLI.runColorTransferAndExit options
