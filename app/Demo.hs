{-# LANGUAGE TemplateHaskell #-}
module Main where

import Brick
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.DropDownMenu
import Control.Category ((>>>))
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
handleEvent s (VtyEvent (EvKey (KChar 'q') [MCtrl])) = halt s
handleEvent s (VtyEvent e) = case focusGetCurrent $ s^.focus of
  Just MenuBar -> if e == EvKey KEsc [] && not (isDropDownMenuOpen (s^.menu))
                  then continue $ s & focus %~ focusSetCurrent Editor
                  else handleDropDownMenuEvent s menu e
  Just Editor -> case e of
    EvKey KEsc [] -> continue $ s & focus %~ focusSetCurrent MenuBar
    _ -> handleEventLensed s edit handleEditorEvent e >>= continue
  Just About -> case e of
    EvKey KEsc [] -> continue $ s & focus %~ focusSetCurrent Editor
    _             -> continue s
  Nothing -> continue s
handleEvent s _ = continue s

exitMenu x = menu %~ closeDropDownMenu
         >>> focus %~ focusSetCurrent x

msg t = set status t >>> exitMenu Editor >>> continue

aboutDialog = exitMenu About >>> continue

menuD =
  [ ("File", FileMenu,
     [ ("Open", FileOpen, msg "Not implemented")
     , ("Quit", FileQuit, halt)])
  , ("Edit", EditMenu,
     [("Cut", EditCut, msg "This could cut something")])
  , ("Help", HelpMenu,
     [("About this program", HelpAbout, aboutDialog)])
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
