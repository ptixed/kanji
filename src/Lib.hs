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
textToBraile text maxw maxh = localGenesis $ do
    image <- charToImage text >>= \x -> resizeImageWithAspect x (2 * maxw) (4 * maxh)
    imageToBraile image

pathToBraile :: T.Text -> IO [[Char]]
pathToBraile path = localGenesis $ do
    image <- undefined
    imageToBraile image

imageToBraile :: MonadResource m => PMagickWand -> m [[Char]]
imageToBraile image = do
    pixels <- imageToPixels image 
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

imageToPixels :: MonadResource m => PMagickWand -> m [[Bool]]
imageToPixels image = do
    h <- getImageHeight image
    (_, it) <- pixelIterator image
    (flip mapM) [1..h] $ \_ -> do
        pixels <- pixelGetNextIteratorRow it
        case pixels of
            Nothing -> return []
            Just xs -> (flip mapM) (V.toList xs) threshold
    where
        threshold :: MonadResource m => PPixelWand -> m Bool
        threshold pixel = do
            c <- pixelGetMagickColor pixel
            r <- getPixelRed   c
            g <- getPixelGreen c
            b <- getPixelBlue  c
            return $ (max r $ max g b) > 0.5

pixelsToBraile :: [[Bool]] -> [[Char]]
pixelsToBraile xs = fmap (\x -> fmap (f x) [0..(length (xs !! 0) `div` 2) - 1]) [0..(length xs `div` 4) - 1] where 
    f ((4*) -> y) ((2*) -> x) = chr $ 0x2800 + 
        (if (xs !! (y + 0) !! (x + 0)) then 1   else 0) +
        (if (xs !! (y + 1) !! (x + 0)) then 2   else 0) +
        (if (xs !! (y + 2) !! (x + 0)) then 4   else 0) +
        (if (xs !! (y + 0) !! (x + 1)) then 8   else 0) +
        (if (xs !! (y + 1) !! (x + 1)) then 16  else 0) +
        (if (xs !! (y + 2) !! (x + 1)) then 32  else 0) +
        (if (xs !! (y + 3) !! (x + 0)) then 64  else 0) +
        (if (xs !! (y + 3) !! (x + 1)) then 128 else 0)
