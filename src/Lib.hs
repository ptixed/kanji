{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( textToImage
    , imageToBraile
    ) where

import Graphics.ImageMagick.MagickCore.Types
import Graphics.ImageMagick.MagickWand

import Data.Text as T

textToImage :: Text -> IO ()
textToImage text = localGenesis $ do
    (_,w) <- magickWand
    (_,dw) <- drawingWand
    pw <- pixelWand

    pw `setColor` "white"
    newImage w 512 256 pw

    dw `setFillColor` pw
    dw `setFont` "Noto-Sans-Mono-CJK-JP-Regular"
    dw `setFontSize` (256 / 0.75)

    pw `setColor` "black"
    dw `setStrokeColor` pw
    dw `setFillColor` pw

    drawAnnotation dw 0 256 text
    drawImage w dw

    writeImage w (Just "a.png")
    
imageToBraile :: Text
imageToBraile = undefined

