{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad

import           Control.Lens ((^.), (.~))
import           Control.Lens.TH (makeLenses)

import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.Widgets.Edit as BE
import qualified Brick.AttrMap as BA

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K

data Event = NoEvent

data Control = Search deriving (Eq, Ord)

data State = State { _stSearchBox :: !(BE.Editor String Control)
                   }

makeLenses ''State

drawUI :: State -> [B.Widget Control]
drawUI st = do
    []

handleEvent :: State -> B.BrickEvent Control Event -> B.EventM Control (B.Next State)
handleEvent st ev = do
    case ev of
        (B.VtyEvent (V.EvKey k ms)) ->
            case (k, ms) of
                (K.KEsc, []) -> B.halt st
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

    let st = State { _stSearchBox = BE.editor Search (Just 1) ""
                   }
    
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    
    void $ B.customMain initialVty buildVty (Nothing) app st

