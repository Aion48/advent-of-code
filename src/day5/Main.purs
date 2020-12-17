module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (any, filter, head, index, last, range, sort)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  
  let 
    boarding :: Array String
    boarding =  String.Utils.lines content

    boardingtickets :: Array (Array Char)
    boardingtickets = map toCharArray boarding

    findvalue :: Array Char -> Int
    findvalue arr = do
      let
        row0 = if index arr 0 == (Just 'B')
                  then 64
                  else 0
        row1 = if index arr 1 == (Just 'B')
                   then 32
                   else 0
        row2 = if index arr 2 == (Just 'B')
                   then 16
                   else 0
        row3 = if index arr 3 == (Just 'B')
                   then 8
                   else 0
        row4 = if index arr 4 == (Just 'B')
                  then 4
                  else 0
        row5 = if index arr 5 == (Just 'B')
                  then  2
                  else 0
        row6 = if index arr 6 == (Just 'B')
                  then 1
                  else 0
      
        row = row0 + row1 + row2 + row3 + row4 + row5 + row6

        column0 = if index arr 7 == (Just 'R')
                    then 4
                    else 0
        column1 = if index arr 8 == (Just 'R')
                    then 2
                    else 0
        column2 = if index arr 9 == (Just 'R')
                    then 1
                    else 0
        
        column = column0 + column1 + column2
      
      (row * 8) + column
  
    highestID :: Int
    highestID = fromMaybe 0 $ last $ sort $ map findvalue boardingtickets
  
    ids :: (Array Int)
    ids = sort $ map findvalue boardingtickets
    
    

    missingseat :: Int -> Boolean
    missingseat i = 
      if (not (any (_ == i) ids))
        then if (any (_ == (i - 1)) ids)
                then if (any (_ == (i + 1)) ids)
                      then true
                      else false
                else false
        else false

    missingseats :: (Array Int)
    missingseats = filter missingseat (0 `range` 1023)


  logShow $ fromMaybe 0 $ head missingseats
  pure unit
