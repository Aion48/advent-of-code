module Main where

import Data.Function.Memoize
import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, head, index, last, length, range, sort, tail)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.BigInt

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  
  let 
    numbers :: Array Int
    numbers = Array.mapMaybe Int.fromString $ String.Utils.lines content
    
    sortednumbers :: Array Int
    sortednumbers = [-2] <> [-1] <> [0] <> (sort numbers)

    temparr :: Array Int
    temparr = 1 `range` ((length sortednumbers) - 1)

    getdifference :: Int -> Int
    getdifference n = (fromMaybe 0 (index sortednumbers n)) - (fromMaybe 0 (index sortednumbers (n - 1)))

    differences :: Array Int
    differences = [fromMaybe 0 (head sortednumbers)] <> (map getdifference temparr) <> [3]

    ones :: Array Int
    ones = filter (_ == 1) differences

    threes :: Array Int
    threes = filter (_ == 3) differences

    checkifheadmatches :: Maybe (Array Int) -> Maybe (Array Int) -> Boolean
    checkifheadmatches _ Nothing = false
    checkifheadmatches Nothing _ = false
    checkifheadmatches (Just arr1) (Just arr2) = 
      if head arr1 == head arr2
        then 
          if (length arr1) == 1
            then true
            else checkifheadmatches (tail arr1) (tail arr2)
        else false

    arrayIncludes ::  Maybe (Array Int) -> Int -> Boolean
    arrayIncludes Nothing _ = false
    arrayIncludes (Just arr) str = 
      if head arr == (Just str)
        then true 
        else arrayIncludes (tail arr) str

    x1 = fromInt 1
    x2 = fromInt 120472576
    x3 = fromInt 240945152
    x4 = fromInt 180708864

    howmanymatches :: (Array Int) -> Int -> BigInt
    howmanymatches _ 0 = x1
    howmanymatches _ 100 = x2
    howmanymatches _ 101 = x3
    howmanymatches _ 102 = x4
    howmanymatches arr1 i = (a + b + c)
      where
        a = if (arrayIncludes (Just arr1) (i - 1  ))
              then howmanymatches arr1 (i - 1)
              else (fromInt 0)
        b = if (arrayIncludes (Just arr1) (i - 2  ))
              then howmanymatches arr1 (i - 2)
              else (fromInt 0)
        c = if (arrayIncludes (Just arr1) (i - 3  ))
              then howmanymatches arr1 (i - 3)
              else (fromInt 0)
      
  
    add2 :: Int
    add2 = 2000000000 + 2000000000  
  --logShow $ fromInt add2
  logShow $  howmanymatches (sortednumbers) (153)
  pure unit
  

