{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ColorTransferSpec where

import ColorTransfer.CLI
import Relude
import System.FilePath
import Test.Syd

images :: [FilePath]
images =
  [ "ales-krivec-4miBe6zg5r0-unsplash.jpg",
    "benjamin-voros-phIFdC6lA4E-unsplash.jpg",
    "jamie-fenn-XhzdJk1za2k-unsplash.jpg",
    "pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg",
    "pine-watt-2Hzmz15wGik-unsplash.jpg",
    "ravi-sharma-f2h77fiLu18-unsplash.jpg",
    "tania-miron-EKX3Lx-t5CM-unsplash.jpg"
  ]

transfers :: [(Int, FilePath, FilePath, FilePath)]
transfers = do
  met <- [1, 2]
  inp <- images
  ref <- images
  guard $ inp /= ref
  let out =
        transformName inp
          <> "--"
          <> transformName ref
          <> "--"
          <> showMethod met
          <.> "jpeg"
  pure
    ( met,
      "./test-resources" </> "originals" </> inp,
      "./test-resources" </> "originals" </> ref,
      "./test-resources" </> "processed" </> out
    )
  where
    transformName a = take (length a - 13) a
    showMethod 1 = "histogram-matching"
    showMethod 2 = "ellipsoid-transformation"
    showMethod _ = ""

spec :: Spec
spec = describe "colortransfer" $ do
  forM_ transfers $ \(me, inputImage, referenceImage, outputImage) -> do
    let goldenFile = outputImage
    let testUnit = runColorTransfer me inputImage referenceImage
    let description =
          toString
            $ unwords
            $ fromString
            <$> [ inputImage,
                  "->",
                  outputImage,
                  "via",
                  if me == 1
                    then "histogram matching"
                    else "ellipsoid transformation"
                ]
    eResult <- liftIO testUnit
    it description $ case eResult of
      Left e -> expectationFailure e
      Right v -> pure $ goldenByteStringFile goldenFile $ pure (toStrict v)
