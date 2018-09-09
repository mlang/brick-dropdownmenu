{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.DropDownMenu (
  DropDownMenu
, dropDownMenu
, handleDropDownMenuEvent, renderDropDownMenu
, isDropDownMenuOpen, closeDropDownMenu
, menuAttr, menuSelectedAttr, listAttr, listSelectedAttr
) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.List
import Data.Foldable (toList)
import Data.List (find, findIndex, intersperse)
import Data.List.PointedList.Circular (PointedList, focus, next, previous, withFocus)
import qualified Data.List.PointedList.Circular as PointedList (fromList)
import qualified Data.Vector as Vector (fromList, length)
import Graphics.Vty (Event(..), Key(..))
import Lens.Micro
import Lens.Micro.TH

type MenuItem s n = (String, n, s -> EventM n (Next s))
type Menu s n = [(String, n, [MenuItem s n])]

data DropDownMenu s n = DropDownMenu {
  _menuName :: n
, _menuOpen :: Bool
, _menuList :: Maybe (PointedList (String, List n (MenuItem s n)))
}

makeLenses ''DropDownMenu

instance Named (DropDownMenu s n) n where getName = (^.menuName)

dropDownMenu :: n -> Menu s n -> DropDownMenu s n
dropDownMenu name desc =
  DropDownMenu name False $ PointedList.fromList $
  (\(t, n, c) -> (t, list n (Vector.fromList c) 1)) <$> desc

handleDropDownMenuEvent
  :: (Eq n, Ord n)
  => s
  -> Lens' s (DropDownMenu s n)
  -> Event
  -> EventM n (Next s)
handleDropDownMenuEvent s target = \case
  EvKey KLeft []  -> continue $ s & target.menuList._Just %~ previous
  EvKey KRight [] -> continue $ s & target.menuList._Just %~ next
  EvKey KEsc []   -> continue $ s & target.menuOpen .~ False
  e@(EvKey KDown [])
    | not $ s^.target.menuOpen -> continue $ s & target.menuOpen .~ True
    | otherwise -> continue =<< handleEventLensed s target handleSubmenuEvent e
  EvKey KEnter []
    | not $ s^.target.menuOpen -> continue $ s & target.menuOpen .~ True
    | otherwise ->
      case s ^? target.menuList._Just.focus._2 >>= listSelectedElement of
        Nothing -> continue s
        Just (_, (_, _, f)) -> f s
  e | s^.target.menuOpen ->
      continue =<< handleEventLensed s target handleSubmenuEvent e
    | otherwise -> continue s

renderDropDownMenu
  :: (Eq n, Ord n, Show n)
  => Bool
  -> DropDownMenu s n
  -> Widget n
renderDropDownMenu focused m = case m^?menuList._Just of
  Nothing -> emptyWidget
  Just menus ->
    let o = m^.menuOpen
        f ((t, l), sel) =
          let cursor = if focused && sel && not o
                       then showCursor (getName l) (Location (0, 0))
                       else id
              attr = if focused && sel && not o
                     then withAttr menuSelectedAttr
                     else id
              height = Vector.length $ listElements l
              width = maximum $ (\(t, _, _) -> textWidth t) <$> listElements l
          in if focused && sel && o
             then borderWithLabel (str t) $
                  padLeftRight 1 $ vLimit height $ hLimit width $
                  renderList drawMenuItem focused l
             else attr $ cursor $ str t
    in hBox $ intersperse (str " ") $ map f (toList (withFocus menus))

handleSubmenuEvent
  :: (Ord n)
  => Event
  -> DropDownMenu s n
  -> EventM n (DropDownMenu s n)
handleSubmenuEvent e m = maybe (pure m) handleEvent $ m ^? menuList._Just where
  handleEvent menus = do
    menus' <- handleEventLensed menus (focus._2) handleListEvent e
    pure $ m & menuList._Just .~ menus'

drawMenuItem :: Bool -> MenuItem s n -> Widget n
drawMenuItem sel (t, n, _) =
  let cursor = if sel then showCursor n (Location (0, 0)) else id
  in cursor $ str t

isDropDownMenuOpen :: DropDownMenu s n -> Bool
isDropDownMenuOpen = (^.menuOpen)

closeDropDownMenu :: DropDownMenu s n -> DropDownMenu s n
closeDropDownMenu = set menuOpen False

menuAttr, menuSelectedAttr :: AttrName
menuAttr = "dropdownmenu"
menuSelectedAttr = menuAttr <> "selected"
