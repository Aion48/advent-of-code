module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (any, length)
import Data.Array as Data.Array
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as String.CodeUnits
import Data.String.Utils (fromCharArray, includes, lines, words)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Async (truncate)
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser.String (string)

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

    aftercolon :: Array Char -> Maybe (Array Char)
    aftercolon arr =
      if fromMaybe ':' (Data.Array.head arr) == ':'
        then Data.Array.tail arr
        else aftercolon (fromMaybe [':'] (Data.Array.tail arr))

    afterchar :: Array Char -> Char -> Maybe (Array Char)
    afterchar arr c =
      if fromMaybe c (Data.Array.head arr) == c
        then Data.Array.tail arr
        else afterchar (fromMaybe [c] (Data.Array.tail arr)) c

    beforechar :: Array Char -> Char -> Array Char
    beforechar arr c = 
      if fromMaybe c (Data.Array.head arr) == c 
        then []
        else  [ (fromMaybe c (Data.Array.head arr)) ] <>  beforechar (fromMaybe [c] (Data.Array.tail arr) ) c

    splitintopassports :: Maybe (Array String) -> Array (Array String)
    splitintopassports Nothing = []
    splitintopassports (Just arr) = [(beforeempty arr)] <> (splitintopassports (afterempty arr))
      
    addwhitespacepre :: String -> String
    addwhitespacepre str = (" " <> str <> " ")

    addwhitespace :: Array (String) -> Array (String)
    addwhitespace str = map (addwhitespacepre) str

    validatedpassports :: Array (Array String) -> Array (Array String)
    validatedpassports arr = do
      j <- arr

      guard (includes ("byr") (fromCharArray j))
      guard (includes ("iyr") (fromCharArray j))
      guard (includes ("eyr") (fromCharArray j))
      guard (includes ("hgt") (fromCharArray j))
      guard (includes ("hcl") (fromCharArray j))
      guard (includes ("ecl") (fromCharArray j))
      guard (includes ("pid") (fromCharArray j))

      pure j
    
    byrcheck :: String -> Boolean
    byrcheck str = 
        if includes ("byr") str
          then 
            if (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) >= 1920 && (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) <= 2002
              then true 
              else false 
          else false

    iyrcheck :: String -> Boolean
    iyrcheck str = 
        if includes ("iyr") str
          then 
            if (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) >= 2010 && (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) <= 2020
              then true 
              else false 
          else false

    eyrcheck :: String -> Boolean
    eyrcheck str = 
        if includes ("eyr") str
          then 
            if (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) >= 2020 && (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) <= 2030
              then true 
              else false 
          else false

    hgtcheck :: String -> Boolean
    hgtcheck str = 
        if includes ("hgt") str
          then 
            if includes ("cm") str
              then
                if (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ beforechar (fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) 'c') >= 150 && (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ beforechar (fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) 'c') <= 193
                  then true 
                  else false 
              else if includes ("in") str
                then if (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ beforechar (fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) 'i') >= 59 && (fromMaybe 0 $ fromString $ String.CodeUnits.fromCharArray $ beforechar (fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) 'i') <= 76
                  then true 
                  else false 
                else false
          else false
    
    hclcheck :: String -> Boolean
    hclcheck str = 
      if includes ("hcl") str
        then 
          if (length (fromMaybe [] (afterchar (String.CodeUnits.toCharArray str) '#'))) == 6
            then true 
            else false 
        else false

    eclcheck :: String -> Boolean
    eclcheck str =
      if includes ("ecl") str
        then 
          if includes ("amb") str || includes ("blu") str || includes ("brn") str || includes ("gry") str || includes ("grn") str || includes ("hzl") str || includes ("oth") str
            then true 
            else false 
        else false

    pidcheck :: String -> Boolean
    pidcheck str =
      if includes ("pid") str
          then 
            if (length  $ fromMaybe [] (aftercolon (String.CodeUnits.toCharArray str))) == 9
              then true 
              else false 
          else false

    validatevalidated :: Array (Array String) -> Array (Array String)
    validatevalidated arr = do
      j <- arr

      guard (any byrcheck j)
      guard (any iyrcheck j)
      guard (any eyrcheck j)
      guard (any hgtcheck j)
      guard (any hclcheck j)
      guard (any eclcheck j)
      guard (any pidcheck j)
      pure j

    spacedoutpassports = map fromCharArray $ validatedpassports $ (map (addwhitespace) $ splitintopassports (Just input))

    wordedpassports = map words spacedoutpassports

    


  logShow $ length $ validatevalidated wordedpassports
  --logShow $ map fromCharArray $ validatedpassports $ splitintopassports (Just input)
  pure unit 


  

