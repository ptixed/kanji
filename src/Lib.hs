{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( charToImage
    , imageToBraile
    ) where

import Graphics.ImageMagick.MagickCore.Types
import Graphics.ImageMagick.MagickWand

import Data.Text as T

charToImage :: Char -> IO ()
charToImage char = localGenesis $ do
    let w = 512
    let h = 512
    let size = (fromIntegral $ min w h) / 2

    (_, image) <- magickWand
    (_, dw) <- drawingWand
    pw <- pixelWand

    pw `setColor` "white"
    dw `setFillColor` pw
    newImage image w h pw

    pw `setColor` "black"
    dw `setStrokeColor` pw
    dw `setFillColor` pw
    dw `setFont` "res/NotoSansCJK-Regular.ttc"
    dw `setFontSize` (size / 0.75)
    drawAnnotation dw (size * 0.5) (size * 1.5) (T.pack [char])

    drawImage image dw
    trimImage image 0

    writeImage image (Just "test.png")
    
    -- (_, it) <- pixelIterator image
    
imageToBraile :: Text
imageToBraile = undefined

