module Main where

import Prelude

import Control.MonadZero (guard)
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
    lines' :: Array String
    lines' = String.Utils.lines content

    linesbrokenup :: Array (Array Char)
    linesbrokenup = map toCharArray lines'
    
    count :: Array (Array Char) -> Int -> Int -> Array Int
    count arr right down = do
      i <- Array.range 0 $ Array.length linesbrokenup

      guard ((mod i down) == 0)
      guard ((fromMaybe '.' (Array.index (fromMaybe [] (Array.index linesbrokenup i)) (mod ((i / down) * right) 31))) == '#')

      pure i

    
  logShow (36 * 74 * 80 * 184 * 62)
  --logShow $ (Array.length $ count linesbrokenup 3 1)
  --logShow ( (Array.length $ count linesbrokenup 1 1) * (Array.length $ count linesbrokenup 3 1) * (Array.length $ count linesbrokenup 5 1) * (Array.length $ count linesbrokenup 7 1) * (Array.length $ count linesbrokenup 1 2)               ) 
  pure unit 

  

