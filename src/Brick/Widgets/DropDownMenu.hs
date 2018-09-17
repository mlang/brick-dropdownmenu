{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Brick.Widgets.DropDownMenu
Description : A drop-down menu type and functions for manipulating and rendering it.
Copyright   : (c) Mario Lang, 2018
License     : BSD-style (see the file LICENSE)
Maintainer  : mlang@blind.guru
Stability   : experimental
Portability : POSIX

This module provides a simple drop-down menu widget with global key bindings.

A submenu consists of a title, a resource name, and an associated global event.
Each menu item consists of the items title, resource name, global event and
associated action.

An action is a function @s -> 'EventM' n ('Next' s)@.
-}
module Brick.Widgets.DropDownMenu (
  DropDownMenu
-- * Constructing a drop-down menu
, dropDownMenu
-- * Handling events
, handleDropDownMenuEvent, handleGlobalDropDownMenuEvent
-- * Rendering drop-down menus
, renderDropDownMenu
-- * Accessors
, isDropDownMenuOpen
-- * Manipulating a drop-down menu
, closeDropDownMenu
-- * Attributes
, menuAttr, menuSelectedAttr
) where

import Brick                                          ( Named(..)
                                                      , AttrName
                                                      , EventM, Next
                                                      , Location(..), Widget
                                                      , continue, emptyWidget
                                                      , handleEventLensed
                                                      , hBox, hLimit
                                                      , padLeftRight, padTopBottom
                                                      , showCursor, str
                                                      , textWidth, vLimit
                                                      , withAttr
                                                      )
import Brick.Widgets.Border                           ( borderWithLabel )
import Brick.Widgets.List                             ( List, list
                                                      , handleListEvent
                                                      , listElements, listMoveTo
                                                      , listSelectedElement
                                                      , renderList
                                                      )
import Data.Foldable                                  ( toList )
import Data.List                                      ( find, findIndex
                                                      , intersperse
                                                      )
import Data.List.PointedList                          ( PointedList
                                                      , focus, moveTo, withFocus
                                                      )
import Data.List.PointedList.Circular                 ( next, previous )
import qualified Data.List.PointedList as PointedList ( fromList )
import Data.Map                                       ( Map )
import qualified Data.Map as Map                      ( empty, fromList )
import Data.Maybe                                     ( fromMaybe, mapMaybe )
import qualified Data.Vector as Vector                ( fromList, length )
import Graphics.Vty                                   ( Event(..)
                                                      , Key(..), Modifier(..)
                                                      )
import Lens.Micro.GHC                                 ( Lens', Traversal'
                                                      , _2, _Just
                                                      , (&), (^.), (^?)
                                                      , (.~), (%~), at, set
                                                      )
import Lens.Micro.TH                                  ( makeLenses )

type MenuItem s n = (String, n, Maybe Event, s -> EventM n (Next s))
type Menu s n = [(String, n, Maybe Event, [MenuItem s n])]
data Action s n = MoveTo !Int | Invoke (s -> EventM n (Next s))

-- | Drop-down menus present a menu bar with drop-down submenus.
--
-- Drop-down menus support the following events by default:
--
-- * Left/right arrow keys: Switch to previous/next submenu
-- * Up arrow key: Close submenu when already at top, otherwise move selection in submenu
-- * Down arrow key: Open submenu or move submenu selection downwards
-- * Escape: Close submenu
-- * Return: Open submenu or invoke selected submenu item
data DropDownMenu s n = DropDownMenu {
  _menuName :: n
, _menuOpen :: Bool
, _menuKeyMap :: Map Event (Action s n)
, _menuList :: Maybe (PointedList (String, List n (MenuItem s n)))
}

makeLenses ''DropDownMenu

submenuList :: Traversal' (DropDownMenu s n) (List n (MenuItem s n))
submenuList = menuList . _Just . focus . _2

instance Named (DropDownMenu s n) n where getName = _menuName

dropDownMenu
  :: n
  -- ^ The resource name for this drop-down menu
  -> [(String, n, Maybe Event, [(String, n, Maybe Event, s -> EventM n (Next s))])]
  -- ^ Menu description
  -> DropDownMenu s n
dropDownMenu name desc =
  DropDownMenu name False keyMap $ PointedList.fromList $
  (\(t, n, _, c) -> (t, list n (Vector.fromList c) 1)) <$> desc
 where
  keyMap = Map.fromList . concat $ zipWith f [0..] desc where
    f i (_, _, Just e, xs) = (e, MoveTo i) : mapMaybe g xs
    f _ (_, _, Nothing, xs) = mapMaybe g xs
    g (_, _, Just e, a) = Just (e, Invoke a)
    g _ = Nothing
    
-- | Handle drop-down menu events.
-- This should typically be called from the application event handler
-- if this menu widget has focus.
handleDropDownMenuEvent
  :: (Eq n, Ord n)
  => s
  -- ^ The application state
  -> Lens' s (DropDownMenu s n)
  -- ^ A lens for accessing the drop-down menu state
  -> (s -> s)
  -- ^ Sets focus to this drop-down menu widget if need be
  -> Event
  -- ^ Event received from Vty
  -> EventM n (Next s)
handleDropDownMenuEvent s target setFocus = \case
  EvKey KLeft []  -> continue $ s & target.menuList._Just %~ previous
  EvKey KRight [] -> continue $ s & target.menuList._Just %~ next
  EvKey KEsc []   -> continue $ s & target.menuOpen .~ False
  EvKey KUp []
    | s^.target.menuOpen &&
      fmap fst (listSelectedElement =<< s ^? target.submenuList) == Just 0 ->
      continue $ s & target.menuOpen .~ False
  EvKey KDown []
    | not $ s^.target.menuOpen ->
      continue $ s & target.menuOpen    .~ True
                   & target.submenuList %~ listMoveTo 0
  EvKey KEnter []
    | not $ s^.target.menuOpen -> continue $ s & target.menuOpen .~ True
    | otherwise ->
      case fmap snd (listSelectedElement =<< s ^? target.submenuList) of
        Nothing -> continue s
        Just (_, _, _, f) -> f s
  e -> let handle = if s ^. target.menuOpen
                    then continue =<< handleEventLensed s target handleSubmenuEvent e
                    else continue s
       in fromMaybe handle $ handleGlobalDropDownMenuEvent s target setFocus e

-- | Handle global events.
-- This function will handle global events associated with submenus
-- or menu items.  It should typically be called from the main
-- application event handler before any other more specific handlers.
handleGlobalDropDownMenuEvent
  :: (Eq n, Ord n)
  => s
  -- ^ The application state
  -> Lens' s (DropDownMenu s n)
  -- ^ A lens for accessing the drop-down menu state
  -> (s -> s)
  -- ^ Set application focus
  -> Event
  -- ^ Event received from Vty
  -> Maybe (EventM n (Next s))
handleGlobalDropDownMenuEvent s target setFocus e = go =<< s ^. target . menuKeyMap . at e where
  go (MoveTo n) = s ^. target.menuList >>= moveTo n >>= \l ->
                  pure . continue $
                  s & target.menuList._Just .~ l
                    & target.menuOpen .~ True
                    & setFocus
  go (Invoke f) = pure $ f s

renderDropDownMenu
  :: (Eq n, Ord n, Show n)
  => Bool
  -- ^ Does this menu have focus?
  -> DropDownMenu s n
  -> Widget n
renderDropDownMenu focused m = maybe emptyWidget render $ m ^. menuList where
  render menus =
    let o = m^.menuOpen
        f ((t, l), sel) =
          let cursor = if focused && sel && not o
                       then showCursor (getName l) (Location (0, 0))
                       else id
              attr = if focused && sel && not o
                     then withAttr menuSelectedAttr
                     else id
              height = Vector.length $ listElements l
              width = maximum $ textWidth . showMenuItem <$> listElements l
          in if focused && sel && o
             then borderWithLabel (str t) $
                  padLeftRight 1 $ vLimit height $ hLimit width $
                  renderList drawMenuItem focused l
             else attr $ cursor $ str t
    in hBox $ intersperse (str " ") $ map f (toList (withFocus menus))

handleSubmenuEvent
  :: Ord n
  => Event
  -> DropDownMenu s n
  -> EventM n (DropDownMenu s n)
handleSubmenuEvent e m = maybe (pure m) handleEvent $ m ^. menuList where
  handleEvent menus = do
    menus' <- handleEventLensed menus (focus._2) handleListEvent e
    pure $ m & menuList._Just .~ menus'

drawMenuItem :: Bool -> MenuItem s n -> Widget n
drawMenuItem sel x@(t, n, e, _) =
  let cursor = if sel then showCursor n (Location (0, 0)) else id
  in cursor $ str $ showMenuItem x

showMenuItem :: MenuItem s n -> String
showMenuItem (t, _, Just e, _)  = t <> " (" <> showEvent e <> ")"
showMenuItem (t, _, Nothing, _) = t

showEvent :: Event -> String
showEvent (EvKey (KChar c) mods) = concatMap showModifier mods <> [c] where
  showModifier MCtrl = "Ctrl-"
  showModifier MMeta = "Meta-"
  showModifier MAlt  = "Alt-"
  showModifier _     = ""

isDropDownMenuOpen :: DropDownMenu s n -> Bool
isDropDownMenuOpen = (^. menuOpen)

-- | Close submenu.
closeDropDownMenu :: DropDownMenu s n -> DropDownMenu s n
closeDropDownMenu = set menuOpen False

menuAttr, menuSelectedAttr :: AttrName
menuAttr = "dropdownmenu"
menuSelectedAttr = menuAttr <> "selected"
