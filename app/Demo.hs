{-# LANGUAGE TemplateHaskell #-}
module Main where

import Brick
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.DropDownMenu
import Brick.Widgets.List (listAttr, listSelectedAttr)
import Control.Category ((>>>))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text
import Graphics.Vty (defAttr, blue, white, Event(EvKey), Key(..), Modifier(..))
import Lens.Micro
import Lens.Micro.TH

data Name = MenuBar
          | FileMenu | FileOpen | FileQuit
          | EditMenu | EditCut
          | HelpMenu | HelpAbout
          | Editor
          | About
          deriving (Eq, Ord, Show)
data State = State {
  _menu :: DropDownMenu State Name
, _edit :: Editor Text Name
, _status :: String
, _focus :: FocusRing Name
}
makeLenses ''State

draw s = mconcat
  [ about $ focusGetCurrent (s^.focus) == Just About
  , [ withFocusRing (s^.focus) renderDropDownMenu (s^.menu)
    , translateBy (Location (0, 1)) $
      vBox [ withFocusRing (s^.focus) (renderEditor (txt . Data.Text.unlines)) (s^.edit)
           , str $ s^.status, str " "
           ]
    ]
  ]

about True = [centerLayer $ borderWithLabel (str "About this program") info]
 where
  info = padLeftRight 2 $ padTopBottom 1 $
         str "This is a demo of the dropdownmenu widget."
     <=> str " "
     <=> showCursor About (Location (0, 0)) (str "Press ESC to continue")
about _ = []


handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s (VtyEvent e) =
  fromMaybe accordingToFocus $
  handleGlobalDropDownMenuEvent s menu (setFocus MenuBar) e
 where
  accordingToFocus = case focusGetCurrent $ s^.focus of
    Just MenuBar -> if e == EvKey KEsc [] && not (isDropDownMenuOpen (s^.menu))
                    then continue $ s & focus %~ focusSetCurrent Editor
                    else handleDropDownMenuEvent s menu (setFocus MenuBar) e
    Just Editor -> case e of
      EvKey KEsc [] -> continue . setFocus MenuBar $ s
      _             -> continue =<< handleEventLensed s edit handleEditorEvent e
    Just About -> case e of
      EvKey KEsc [] -> continue . setFocus Editor $ s
      _             -> continue s
    Nothing -> continue s
handleEvent s _ = continue s

setFocus n = focus %~ focusSetCurrent n

exitMenu x = menu %~ closeDropDownMenu
         >>> focus %~ focusSetCurrent x

msg t = set status t >>> exitMenu Editor >>> continue

aboutDialog = exitMenu About >>> continue

menuD =
  [ ("File", FileMenu, Just (EvKey (KChar 'f') [MMeta]), 
     [ ("Open", FileOpen, Just (EvKey (KChar 'o') [MCtrl]), msg "Not implemented")
     , ("Quit", FileQuit, Just (EvKey (KChar 'q') [MCtrl]), halt)])
  , ("Edit", EditMenu, Just (EvKey (KChar 'e') [MMeta]),
     [("Cut", EditCut, Just (EvKey (KChar 'x') [MCtrl]), msg "This could cut something")])
  , ("Help", HelpMenu, Just (EvKey (KChar 'h') [MMeta]),
     [("About this program", HelpAbout, Nothing, aboutDialog)])
  ] 

demo = defaultMain
  App {
  appChooseCursor = showFirstCursor
, appDraw = draw
, appAttrMap = const attrs
, appStartEvent = pure
, appHandleEvent = handleEvent
}

main = demo State {
  _menu = dropDownMenu MenuBar menuD
, _edit = editor Editor Nothing (pack "You can change this text.\nPress ESC to go to the menu.")
, _status = "started."
, _focus = focusRing [Editor, About, MenuBar]
}

attrs = attrMap defAttr
  [ (menuAttr,         white `on` blue)
  , (menuSelectedAttr, blue `on` white)
  , (listAttr,         white `on` blue)
  , (listSelectedAttr, blue `on` white)
  ]
