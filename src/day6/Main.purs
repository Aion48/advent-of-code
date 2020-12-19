module Main where

import Prelude

import Control.Monad.List.Trans (foldl)
import Control.MonadZero (guard)
import Data.Array (concat, head, intercalate, length, nub, tail)
import Data.Array as Array
import Data.Array as Data.Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Async (truncate)
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
        
    splitintogroups :: Maybe (Array String) -> Array (Array String)
    splitintogroups Nothing = []
    splitintogroups (Just arr) = [(beforeempty arr)] <> (splitintogroups (afterempty arr))

    groups :: Array (Array String)
    groups = splitintogroups (Just input)

    {-grouptochars :: Array String -> Array Char
    grouptochars arr = concat (map toCharArray arr) 

    groups' :: Array (Array Char)
    groups' = map grouptochars groups
    
    groupscleaned :: Array (Array Char)
    groupscleaned = map nub groups'

    groupsall :: Array Char
    groupsall = concat groupscleaned

    totalcount :: Int
    totalcount = length groupsall -}

    grouptochars :: Array String -> Array (Array Char)
    grouptochars arr = (map toCharArray arr) 

    groups' :: Array (Array (Array Char))
    groups' = map grouptochars groups
    
    arrayIncludes ::  Maybe (Array Char) -> Char -> Boolean
    arrayIncludes Nothing _ = false
    arrayIncludes (Just arr) char = 
      if head arr == (Just char)
        then true 
        else arrayIncludes (tail arr) char

    checkrestofgroup :: Array (Array Char) -> Maybe Char -> Array (Array Char)
    checkrestofgroup _ Nothing = []
    checkrestofgroup arr (Just cha) = do
        j <- arr 

        guard $ arrayIncludes (Just j) cha
        pure j 

    checkallgroupmembersagainstfirst :: Maybe (Array Char) -> Array (Array Char) -> Int
    checkallgroupmembersagainstfirst Nothing _ = 0
    checkallgroupmembersagainstfirst (Just arr) farr = 
      if length (checkrestofgroup farr ((head arr)) ) == length farr
        then 1 + (checkallgroupmembersagainstfirst (tail arr) farr)
        else 0 + (checkallgroupmembersagainstfirst (tail arr) farr)

    grouptoscore :: Array (Array Char) -> Int
    grouptoscore arr = checkallgroupmembersagainstfirst (head arr) arr

    totalscore :: Int 
    totalscore = sum (map grouptoscore groups' )
    --checkgroup :: Maybe (Array (Array Char)) -> String
    --checkgroup Nothing = []
    --checkgroup (Just arr) = 

  --logShow totalcount
  logShow totalscore
  pure unit

