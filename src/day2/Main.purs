module Main where

import Data.Maybe
import Prelude

import Control.MonadZero (guard)
import Data.Array as Data.Array
import Data.Array.NonEmpty (appendArray)
import Data.Int (fromString)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Async (truncate)
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do 
  content <- readTextFile UTF8 "input.txt"
  

  let 
    passwords :: Array String
    passwords =  String.Utils.lines content

    passwordscutup :: Array (Array Char)
    passwordscutup = map toCharArray passwords

    
    beforecolon :: Array Char -> Array Char
    beforecolon arr = 
      if fromMaybe ':' (Data.Array.head arr) == ':'
        then []
        else  [ (fromMaybe ' ' (Data.Array.head arr)) ] <>  beforecolon (fromMaybe [':'] (Data.Array.tail arr))

    aftercolon :: Array Char -> Array Char
    aftercolon arr =
      if fromMaybe ':' (Data.Array.last arr) == ':'
        then []
        else  [ (fromMaybe ' ' (Data.Array.last arr)) ] <>  aftercolon (fromMaybe [':'] (Data.Array.init arr))

    beforewhitespace :: Array Char -> Array Char
    beforewhitespace arr = 
      if fromMaybe ' ' (Data.Array.head arr) == ' '
        then []
        else  [ (fromMaybe ' ' (Data.Array.head arr)) ] <>  beforewhitespace (fromMaybe [' '] (Data.Array.tail arr))

    beforedash :: Array Char -> Array Char
    beforedash arr = 
      if fromMaybe '-' (Data.Array.head arr) == '-'
        then []
        else  [ (fromMaybe ' ' (Data.Array.head arr)) ] <>  beforedash (fromMaybe ['-'] (Data.Array.tail arr))
      
    afterdash :: Array Char -> Array Char
    afterdash arr =
      if fromMaybe '-' (Data.Array.last arr) == '-'
        then []
        else  [ (fromMaybe ' ' (Data.Array.last arr)) ] <>  afterdash (fromMaybe ['-'] (Data.Array.init arr))


    passwordworks :: Int -> Int -> Char -> Array Char -> Boolean
    passwordworks firstp secondp letter arr =
      if (Data.Array.index arr (firstp) == (Just letter) || Data.Array.index arr (secondp) == (Just letter) ) && (((Data.Array.index arr (firstp) == (Just letter)) == false) || ( false == (Data.Array.index arr (secondp) == (Just letter))) )
        then true 
        else false

    workingpasswords :: Array (Array Char) -> Array (Array Char)
    workingpasswords arr = do
        i <- arr

        guard $ passwordworks (fromMaybe 0 $ fromString $ fromCharArray $ beforedash $ beforewhitespace $ beforecolon i) (fromMaybe 0 $ fromString $ fromCharArray $  Data.Array.reverse $ afterdash $ beforewhitespace $ beforecolon i) (fromMaybe ' ' $ Data.Array.last $ beforecolon i) ( Data.Array.reverse $ aftercolon i)
        pure i

  pure unit
  --logShow $ aftercolon [' ','a',':',' ','1','2','3','4','5']
  logShow $ Data.Array.length $ workingpasswords passwordscutup