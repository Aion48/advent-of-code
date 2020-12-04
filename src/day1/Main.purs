module Main where

import Prelude

import Control.Comonad.Env (local)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Stream (onFinish)

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
      guard $ a + b == 2020 
      pure $ a * b
    
    result :: Maybe Int
    result = Array.head summed
  
  case result of
    Nothing -> 
      log "No Result Found"
    Just n -> 
      logShow n

