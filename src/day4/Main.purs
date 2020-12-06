module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array as Data.Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (fromCharArray, includes, lines)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"

  let 
    input :: Array String
    input = (String.Utils.lines content) <> [""]

    beforeempty :: Array String -> Array String
    beforeempty arr = 
      if fromMaybe "" (Data.Array.head arr) == "" 
        then []
        else  [ (fromMaybe "" (Data.Array.head arr)) ] <>  beforeempty (fromMaybe [""] (Data.Array.tail arr))
    
    afterempty :: Array String -> Maybe (Array String)
    afterempty arr =
      if fromMaybe "" (Data.Array.head arr) == ""
        then Data.Array.tail arr
        else afterempty (fromMaybe [""] (Data.Array.tail arr))

    splitintopassports :: Maybe (Array String) -> Array (Array String)
    splitintopassports Nothing = []
    splitintopassports (Just arr) = [(beforeempty arr)] <> (splitintopassports (afterempty arr))
      
    validatedpassports :: Array (Array String) -> Array (Array String)
    validatedpassports arr = do
      i <- arr

      guard (includes ("byr") (fromCharArray i))
      guard (includes ("iyr") (fromCharArray i))
      guard (includes ("eyr") (fromCharArray i))
      guard (includes ("hgt") (fromCharArray i))
      guard (includes ("hcl") (fromCharArray i))
      guard (includes ("ecl") (fromCharArray i))
      guard (includes ("pid") (fromCharArray i))

      pure i


  logShow $ Data.Array.length $ validatedpassports $ splitintopassports (Just input)
  pure unit 

  

