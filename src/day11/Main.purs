module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, head, index, length, range, tail, updateAt)
import Data.Array as Array
import Data.Foldable (sum)
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
    occupiedneighbors x y arr = length do
                        xaxis <- [-1,0,1]
                        yaxis <- [-1,0,1]

                        guard $ not (xaxis == 0 && yaxis == 0)
                        guard $ (fromMaybe '.' (index (fromMaybe [] (index arr (y + yaxis)))  (x + xaxis) )) == '#'

                        pure [xaxis,yaxis]

        

    runstep :: Array (Array Char) -> Array (Array Char)
    runstep arr = newgrid
      where
        turnToOccupied = do
                        x <- 0 `range` ((length (fromMaybe [] (head arr))) - 1)
                        y <- 0 `range` ((length arr) - 1)
                        
                        guard ((fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y)) ) (x))) == 'L')
                        guard ((occupiedneighbors x y arr) == 0)

                        pure [x,y]
        turnToEmpty = do
                        x <- 0 `range` ((length (fromMaybe [] (head arr))) - 1)
                        y <- 0 `range` ((length arr) - 1)
                        
                        guard ((fromMaybe 'x' (index ( fromMaybe [] (index (arr) (y)) ) (x))) == '#')
                        guard ((occupiedneighbors x y arr) > 3)

                        pure [x,y]     
        
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
        
    findendstate :: Array (Array Char) -> Array (Array Char)
    findendstate arr =
                  if arr == (runstep arr) 
                        then arr 
                        else findendstate $ runstep arr

    occupiedinrow :: Array Char -> Int
    occupiedinrow arr = length (filter (_ == '#' )arr )

    countoccupied :: Array (Array Char) -> Int
    countoccupied arr = sum (map occupiedinrow arr)

  logShow $ countoccupied $ findendstate grid
  pure unit 
