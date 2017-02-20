module Types where

import qualified Data.Map.Strict as Map
import Data.List.Split (chunksOf)
import Data.List (intercalate)

emptyChar = ' '
borderChar = '#'
mblockChar = 'x'

type Grid = Map.Map (Int,Int) Char
type Width = Int
type Height = Int
type Speed = Float

data Point = Point { getX :: Float, getY :: Float } deriving Show

data VField = VField { 
  vf_width :: Width, 
  vf_height :: Height,
  vf_grid :: Grid
} deriving Show

data GameItem = 
  MBlock {
    mb_width :: Width,
    mb_height :: Height,
    mb_top :: Point,
    mb_speed :: Speed
  } |
  Tmp Int 
  deriving Show


data GameField = GameField {
  gf_width :: Width, 
  gf_height :: Height,
  gf_items :: [GameItem]
} deriving Show


createEmptyGridWithBorders :: Width -> Height -> Grid
createEmptyGridWithBorders w h = let
  bordersCells = [((x, y), borderChar) | 
    y <- [1..h], x <- [1..w], 
    y == 1 || y == h || x == 1 || x == w ]
  emptyCells = [((x, y), emptyChar) | 
    y <- [1..h], x <- [1..w], 
    y /= 1 && y /= h && x /= 1 && x /= w]
  in Map.fromList $ bordersCells ++ emptyCells

createEmptyVField :: Width -> Height -> VField
createEmptyVField w h = VField w h (createEmptyGridWithBorders w h)

updateVField :: VField -> (Int, Int) -> Char -> VField
updateVField oldVf@(VField w h grid) key@(x, y) val = 
  if x <= 1 || x >= w || y <= 1 || y >= h
    then oldVf
    else VField w h (Map.insert key val grid)

vFieldToString :: VField -> String
vFieldToString (VField width height grid) = let
  coords = [(x,y) | y <- [1..height], x <- [1..width]]
  mapFn xy = case (Map.lookup xy grid) of
    (Just c) -> c
    Nothing -> 'E'
  lines = chunksOf width (map mapFn coords)
  in intercalate "\n" lines

vFieldToLines :: VField -> [String]
vFieldToLines (VField width height grid) = let
  coords = [(x,y) | y <- [1..height], x <- [1..width]]
  mapFn xy = case (Map.lookup xy grid) of
    (Just c) -> c
    Nothing -> 'E'
  lines = chunksOf width (map mapFn coords)
  in lines





recalcBlockPos :: GameItem -> GameItem
recalcBlockPos mb = let
  speed = mb_speed mb
  (Point topX topY) = mb_top mb
  in mb { mb_top = Point (topX + speed) topY }

recalcBlocks :: [GameItem] -> [GameItem]
recalcBlocks items = let
  recalcFn mb@(MBlock a b c d) = recalcBlockPos mb
  recalcFn gi = gi
  in map recalcFn items

recalcGameFieldItems :: GameField -> GameField
recalcGameFieldItems (GameField w h items) = let
  newItems = recalcBlocks items
  in GameField w h newItems


tstD vf (MBlock w h (Point x y) _) = let
  (intX, intY) = ((floor x) + 1, (floor y) + 1)
  coords = [(x, y) | y <- [intY..(h+intY-1)], x <- [intX..(w+intX-1)]]
  in coords

drawBlock :: VField -> GameItem -> VField
drawBlock vf (MBlock w h (Point x y) _) = let
  (intX, intY) = ((floor x) + 1, (floor y) + 1)
  coords = [(x, y) | y <- [intY..(h+intY-1)], x <- [intX..(w+intX-1)]]
  in foldr (\xy oldvf -> updateVField oldvf xy mblockChar) vf coords

gFieldToVField :: GameField -> VField
gFieldToVField (GameField gwidth gheight items) = let
  vwidth = gwidth + 2
  vheight = gheight + 2
  emptyVField = createEmptyVField vwidth vheight
  foldFn item oldVf = drawBlock oldVf item
  newVField = foldr foldFn emptyVField items
  in newVField