{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Lib

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text as T
import           Data.List
import           System.Environment

import           Control.Lens
import           Control.Lens.TH (makeLenses)

import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.Center as BB
import qualified Brick.BChan as BC

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K

data Event = TickEvent

data Control = Search deriving (Eq, Ord, Show)

data State = State { _stImage :: [T.Text]
                   , _stFrame :: Int
                   , _stWidth :: Int
                   }

makeLenses ''State

drawUI :: State -> [B.Widget Control]
drawUI st = do
    [B.hLimit (st ^. stWidth) $ BB.hCenter $ contentBlock]
    where
        contentBlock = B.txt $ (st ^. stImage) !! (st ^. stFrame `rem` length (st ^. stImage))

handleEvent :: State -> B.BrickEvent Control Event -> B.EventM Control (B.Next State)
handleEvent st ev = do
    case ev of
        (B.AppEvent (TickEvent)) ->
            B.continue $ over stFrame (+1) st
        (B.VtyEvent (V.EvKey k ms)) ->
            case (k, ms) of
                (K.KEsc, _) -> B.halt st
                _ -> B.continue st
        _ -> B.continue st

main :: IO ()
main = do
    [path, threshold] <- getArgs
    vty <- V.mkVty V.defaultConfig
    (w, h) <- V.displayBounds $ V.outputIface vty

    image <- pathToBraile (T.pack path) w h (read threshold) >>= \xs -> do
        return $ map (T.pack . intercalate "\n") xs

    chan <- BC.newBChan 5
    void . forkIO $ forever $ do
        BC.writeBChan chan TickEvent
        threadDelay 50000

    let appAttrMap = BA.attrMap V.defAttr []

    let app = B.App { B.appDraw = drawUI
                    , B.appChooseCursor = B.showFirstCursor
                    , B.appHandleEvent = handleEvent
                    , B.appStartEvent = pure
                    , B.appAttrMap = const appAttrMap
                    }

    let st = State { _stImage = image
                   , _stFrame = 0
                   , _stWidth = w
                   }
    
    void $ B.customMain (return vty) (Just chan) app st

