module Main where

import Prelude

import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do 
  content <- readTextFile UTF8 "input.txt"
  

  let 
    passwords :: Array String
    passwords =  String.Utils.lines content


  pure unit
  logShow passwords