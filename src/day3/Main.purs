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
    
    count :: Array (Array Char) -> Array Int
    count arr = do
      i <- Array.range 0 $ Array.length linesbrokenup

      guard ((fromMaybe '.' (Array.index (fromMaybe [] (Array.index linesbrokenup i)) (mod (i * 3) 31))) == '#')

      pure i

  --logShow (Array.index (fromMaybe [] (Array.index linesbrokenup 1)) (mod (1 * 3) 31))
  logShow $ Array.length $ count linesbrokenup
  pure unit 

  

