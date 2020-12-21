module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (any, filter, head, index, length, null, range, tail)
import Data.Array as Array
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), indexOf, split, splitAt)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Utils (includes, lines)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Bag =
    { design :: String
    , contains :: (Array String)
    , respectivecounts :: (Array Int)
    }

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  
  let 

    arrayIncludes ::  Maybe (Array String) -> String -> Boolean
    arrayIncludes Nothing _ = false
    arrayIncludes (Just arr) str = 
      if head arr == (Just str)
        then true 
        else arrayIncludes (tail arr) str
    
    rules :: Array String
    rules = lines content

    getDesign :: String -> String
    getDesign str = fromMaybe "" $ head (split (Pattern " bags contain ") str)
    
    getContents :: String -> (Array String)
    getContents "no other bags." = []
    getContents str = 
            if (includes "," str) && ((fromMaybe 0 (fromString (singleton (fromMaybe ' ' (index (toCharArray str) 0))))) > 1)
              then [(splitAt 2 (fromMaybe "" (index (split (Pattern " bags,") str) 0)) ).after ] <> getContents (splitAt  ( 2 + (fromMaybe 0 (indexOf (Pattern ",") str)) ) str ).after
              else 
                if includes "," str
                  then [(splitAt 2 (fromMaybe "" (index (split (Pattern " bag,") str) 0)) ).after ] <> getContents (splitAt  ( 2 + (fromMaybe 0 (indexOf (Pattern ",") str)) ) str ).after
                  else [ fromMaybe "" (index (split (Pattern " bag") ((splitAt 2 str).after)) 0) ]

    getCounts :: String -> Array Int
    getCounts "no other bags." = []
    getCounts str = 
            if includes "s," str
              then [ fromMaybe 0 (fromString (singleton (fromMaybe ' ' (index (toCharArray str) 0))))] <> getCounts (splitAt  ( 2 + (fromMaybe 0 (indexOf (Pattern ",") str)) ) str ).after
              else 
                if includes "," str
                  then [fromMaybe 0 (fromString (singleton (fromMaybe ' ' (index (toCharArray str) 0))))] <> getCounts (splitAt  ( 2 + (fromMaybe 0 (indexOf (Pattern ",") str)) ) str ).after
                  else [fromMaybe 0 (fromString (singleton (fromMaybe ' ' (index (toCharArray str) 0))))] 

    ruleToBag :: String -> Bag
    ruleToBag str = { design: (getDesign str)
                    , contains: (getContents (fromMaybe "" $ (index (split (Pattern " bags contain ") str) 1) ))
                    , respectivecounts: (getCounts (fromMaybe "" $ (index (split (Pattern " bags contain ") str) 1) )) }

    bags :: (Array Bag)
    bags = map ruleToBag rules 

    canContain :: String -> Bag -> Boolean
    canContain str bg = 
      if null bg.contains
        then false 
        else 
          if arrayIncludes (Just bg.contains) str
            then true 
            else isJust $ head $ do
              i <- bg.contains
              j <- bags

              guard (i == j.design)

              guard (canContain str j)

              pure j

    howmanybagscancontain :: String -> (Array Bag) -> (Array Bag)
    howmanybagscancontain str = filter (canContain str)
      
    --part 2

    multiplyarrays :: Maybe (Array Int) -> Maybe (Array Int) -> (Array Int)
    multiplyarrays Nothing _ = []
    multiplyarrays _ Nothing = []
    multiplyarrays (Just arr1) (Just arr2) = [ fromMaybe 0 (head arr1) * fromMaybe 0 (head arr2) ] <> (multiplyarrays (tail arr1) (tail arr2))

    sumArray :: Maybe (Array Int) -> Int
    sumArray Nothing = 0
    sumArray (Just arr) = (fromMaybe 0 (head arr)) + (sumArray (tail arr))

    noBag :: Bag
    noBag = { contains: [], design: "", respectivecounts: [] }

    namematchesdesign :: String -> Bag -> Boolean
    namematchesdesign str bg = (str == bg.design)

    nametobag :: String -> Bag
    nametobag str = fromMaybe noBag $ head (filter (namematchesdesign str) bags)

    cancontainhowmanybags :: String -> Int
    cancontainhowmanybags str = 
      if null (nametobag str).contains
        then 1/2
        else sumArray (Just (nametobag str).respectivecounts) + sumArray (Just ((multiplyarrays (Just (nametobag str).respectivecounts) (Just (map cancontainhowmanybags (nametobag str).contains)) )))

  --logShow bags
  --logShow $ canContain "shiny gold" { contains: ["shiny gold"], design: "bright white", respectivecounts: [1] }
  logShow $ cancontainhowmanybags "shiny gold"
  pure unit
    
    

