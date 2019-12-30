{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Lib
    ( textToBraile
    , pathToBraile
    ) where

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand

import           Control.Monad
import           Control.Monad.Trans.Resource.Internal (MonadResource)

import           Data.Char
import qualified Data.Text as T
import qualified Data.Vector.Storable as V

textToBraile :: Char -> Int -> Int -> IO [[Char]] -- change to Text, does it have to be IO?
textToBraile text maxw maxh = withMagickWandGenesis $ localGenesis $ do
    image <- charToImage text
    imageToBraile image maxw maxh 0.5

pathToBraile :: T.Text -> Int -> Int -> Double -> IO [[[Char]]] 
pathToBraile path maxw maxh thres = withMagickWandGenesis $ localGenesis $ do -- genesis?
    (_, image0) <- magickWand
    readImage image0 path
    
    (_, image1) <- coalesceImages image0
    n <- getNumberImages image1

    forM [0..(n-1)] $ \i -> do
        image1 `setIteratorIndex` i
        (_, image2) <- getImage image1
        imageToBraile image2 maxw maxh thres

imageToBraile :: MonadResource m => PMagickWand -> Int -> Int -> Double -> m [[Char]]
imageToBraile image maxw maxh thres = do
    resized <- resizeImageWithAspect image (2 * maxw) (4 * maxh) 
    pixels <- imageToPixels resized thres
    return $ pixelsToBraile pixels

charToImage :: MonadResource m => Char -> m PMagickWand
charToImage char = do
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
    
    return image

resizeImageWithAspect :: MonadResource m => PMagickWand -> Int -> Int -> m PMagickWand
resizeImageWithAspect image (fromIntegral -> maxw) (fromIntegral -> maxh) = do
    w <- getImageWidth  image >>= return . fromIntegral
    h <- getImageHeight image >>= return . fromIntegral

    let aspect  = w    / h
    let maspect = maxw / maxh

    let scale = if aspect > maspect then maxw / w else maxh / h

    let neww = if aspect > maspect then scale * w else maxw
    let newh = if aspect < maspect then scale * h else maxh

    resizeImage image (floor neww) (floor newh) undefinedFilter 1.0
    
    return image

imageToPixels :: MonadResource m => PMagickWand -> Double -> m [[Int]]
imageToPixels image thres = do
    h <- getImageHeight image
    (_, it) <- pixelIterator image
    (flip mapM) [1..h] $ \_ -> do
        pixels <- pixelGetNextIteratorRow it
        case pixels of
            Nothing -> return []
            Just xs -> (flip mapM) (V.toList xs) $ \pixel -> do
                (_, _, l) <- getHSL pixel
                return $ if l > thres then 1 else 0

pixelsToBraile :: [[Int]] -> [[Char]]
pixelsToBraile (x1:x2:x3:x4:xs) = (f x1 x2 x3 x4):(pixelsToBraile xs) where
    f (y11:y12:ys1) (y21:y22:ys2) (y31:y32:ys3) (y41:y42:ys4) = c:(f ys1 ys2 ys3 ys4) where
        c = chr $ 0x2800 + y11 + y21 * 2 + y31 * 4 + y12 * 8 + y22 * 16 + y32 * 32 + y41 * 64 + y42 * 128
    f _ _ _ _ = []
pixelsToBraile _ = []
