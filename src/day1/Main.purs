module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  
  let 
    numbers :: Array Int
    numbers = Array.mapMaybe Int.fromString $ String.Utils.lines content
    
    summed :: Array Int
    summed = do
      a <- numbers
      b <- numbers
      c <- numbers
      guard $ a + b + c == 2020 
      pure $ a * b * c
    
    result :: Maybe Int
    result = Array.head summed
  
  case result of
    Nothing -> 
      log "No Result Found"
    Just n -> 
      logShow n

