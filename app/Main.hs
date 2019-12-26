{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib

import           Control.Monad
import           Data.Text as T

import           Control.Lens ((^.), (.~))
import           Control.Lens.TH (makeLenses)

import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.Widgets.Edit as BE
import qualified Brick.AttrMap as BA

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K

data Event = NoEvent

data Control = Search deriving (Eq, Ord, Show)

data State = State { _stSearchBox :: !(BE.Editor Text Control)
                   }

makeLenses ''State

drawUI :: State -> [B.Widget Control]
drawUI st = do
    [B.padAll 1 contentBlock]
    where
        contentBlock = editor Search (st ^. stSearchBox) 
        editor n e =
            B.vLimit 1 $
            BE.renderEditor True e

handleEvent :: State -> B.BrickEvent Control Event -> B.EventM Control (B.Next State)
handleEvent st ev = do
    case ev of
        (B.VtyEvent (V.EvKey k ms)) ->
            case (k, ms) of
                (K.KEsc, _) -> B.halt st
                _ -> B.continue st
        _ -> B.continue st

main :: IO ()
main = do
    let appAttrMap = BA.attrMap V.defAttr [ (BE.editAttr,        V.white `B.on` V.black)
                                          , (BE.editFocusedAttr, V.black `B.on` V.yellow)
                                          ]

    let app = B.App { B.appDraw = drawUI
                    , B.appChooseCursor = B.showFirstCursor
                    , B.appHandleEvent = handleEvent
                    , B.appStartEvent = pure
                    , B.appAttrMap = const appAttrMap
                    }

    let st = State { _stSearchBox = BE.editor Search (B.txt . T.unlines)  (Just 1) ""
                   }
    
    void $ B.customMain (V.mkVty V.defaultConfig) (Nothing) app st

