module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (head, index, length, range, tail, updateAt)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  
  let 
    lined = lines content

    grid = map (toCharArray) lined

    occupiedneighbors :: Int -> Int -> Array (Array Char) -> Int
    occupiedneighbors x y arr = a + b + c + d + e + f + g + h 
      where
        a = if (fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y - 1)) ) (x - 1))) == '#'
              then 1
              else 0
        b = if (fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y - 1)) ) (x))) == '#'
              then 1
              else 0
        c = if (fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y - 1)) ) (x + 1))) == '#'
              then 1
              else 0
        d = if (fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y)) ) (x - 1))) == '#'
              then 1
              else 0
        e = if (fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y)) ) (x + 1))) == '#'
              then 1
              else 0
        f = if (fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y + 1)) ) (x - 1))) == '#'
              then 1
              else 0
        g = if (fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y + 1)) ) (x))) == '#'
              then 1
              else 0
        h = if (fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y + 1)) ) (x + 1))) == '#'
              then 1
              else 0

    runstep :: Array (Array Char) -> Array (Array Char)
    runstep arr = newgrid
      where
        turnToOccupied = do
                        x <- 0 `range` ((length (fromMaybe [] (head arr))) - 1)
                        y <- 0 `range` ((length arr) - 1)
                        
                        guard ((fromMaybe 'x' (index ( fromMaybe [] (index (arr) (x)) ) (y))) == 'L')
                        guard ((occupiedneighbors x y arr) == 0)

                        pure [y,x]
        turnToEmpty = do
                        x <- 0 `range` ((length (fromMaybe [] (head arr))) - 1)
                        y <- 0 `range` ((length arr) - 1)
                        
                        guard ((fromMaybe 'x' (index ( fromMaybe [] (index (arr) (x)) ) (y))) == '#')
                        guard ((occupiedneighbors x y arr) > 3)

                        pure [y,x]     
        
        changearrtoOccupied :: Array (Array Char) -> Array (Array Int) -> Array (Array Char)
        changearrtoOccupied arr1 arr2 = 
            if ((length arr2) == 0)
              then arr1
              else changearrtoOccupied (fromMaybe [] (updateAt (fromMaybe 0 (index (fromMaybe [] (head arr2)) 1 )) (fromMaybe []( updateAt (fromMaybe 0 (index (fromMaybe [] (head arr2)) 0 )) '#' (fromMaybe [] (index arr1 (fromMaybe 0 (index (fromMaybe [] (head arr2)) 1 )))))) arr1)) (fromMaybe [] (tail arr2))

        changearrtoEmpty :: Array (Array Char) -> Array (Array Int) -> Array (Array Char)
        changearrtoEmpty arr1 arr2 = 
            if ((length arr2) == 0)
              then arr1
              else changearrtoEmpty (fromMaybe [] (updateAt (fromMaybe 0 (index (fromMaybe [] (head arr2)) 1 )) (fromMaybe []( updateAt (fromMaybe 0 (index (fromMaybe [] (head arr2)) 0 )) 'L' (fromMaybe [] (index arr1 (fromMaybe 0 (index (fromMaybe [] (head arr2)) 1 )))))) arr1)) (fromMaybe [] (tail arr2))
        
        newgrid1 = (changearrtoOccupied arr turnToOccupied)
        newgrid = (changearrtoEmpty newgrid1 turnToEmpty)
        



  logShow $ runstep $ runstep grid
  pure unit 
