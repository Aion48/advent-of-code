module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, head, index, last, length, null, range, sort)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
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
    
    nbeforexarray :: Int -> Int -> Array Int -> Array Int
    nbeforexarray 0 _ _ = []
    nbeforexarray n x arr = [fromMaybe 0 (index arr ((x) - n))] <> (nbeforexarray (n - 1) x arr)
    
    checkforrulebreak :: Int -> Boolean
    checkforrulebreak x = 
      if (x > 25)
        then null $ do
          i <- (nbeforexarray 25 x numbers)
          j <- (nbeforexarray 25 x numbers)

          guard (not (i == j))
          guard ((i + j) == (fromMaybe 0 (index numbers x)) )

          pure [i,j]
        else false

    indexofbreaks = filter checkforrulebreak (0 `range` (length numbers))

    firstbreak = fromMaybe 0 (index numbers (fromMaybe 0 (head indexofbreaks)))

    lengthofnumbers = ((length numbers) - 1)

    previousxsumsupto :: Int -> Int -> Int -> Boolean
    previousxsumsupto pre index check = (sum (nbeforexarray pre index numbers)) == check

    checknumbersforconsequtivex :: Int -> Int -> Array Int
    checknumbersforconsequtivex x ntocheckf = check' x ntocheckf 0
      where 
        check' :: Int -> Int -> Int -> Array Int
        check' _ _ 500 = []
        check' x' n count = 
          if (previousxsumsupto x' count ntocheckf)
            then (nbeforexarray x' count numbers)
            else check' x' ntocheckf (count + 1)
    
    checknumbers :: Int -> Array Int
    checknumbers lengthtocheck = 
      if (lengthtocheck > lengthofnumbers)
        then [lengthtocheck]
        else
          if null (checknumbersforconsequtivex lengthtocheck firstbreak)
            then checknumbers (lengthtocheck + 1)
            else (checknumbersforconsequtivex lengthtocheck firstbreak)

  logShow (fromMaybe 0 (head (sort (checknumbers 2))) + fromMaybe 0 (last (sort (checknumbers 2))))
  pure unit
