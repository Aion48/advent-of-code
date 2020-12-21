module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (head, index, last, null, tail)
import Data.Array as Array
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.Utils (lines)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Instruction = {
                    type :: String
                  , number :: Int
                    }

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  
  let 
    emptyInstruction = {
                    type: ""
                  , number: 0
                    }

    instructions :: Array String
    instructions = lines content

    readinstruction :: String -> Instruction
    readinstruction str = {
                    type: fromMaybe "" (index (split (Pattern " ") str) 0)
                  , number: fromMaybe 0 (fromString (fromMaybe "" (index (split (Pattern " ") str) 1)))
                    }

    instructions' :: Array Instruction
    instructions' = map readinstruction instructions

    arrayIncludes ::  Maybe (Array Int) -> Int -> Boolean
    arrayIncludes Nothing _ = false
    arrayIncludes (Just arr) str = 
      if head arr == (Just str)
        then true 
        else arrayIncludes (tail arr) str

    runInstructions ::  Array Instruction -> Int
    runInstructions arr = runInstructions' arr 0 [] 0
      where
        runInstructions' :: Array Instruction -> Int -> Array Int -> Int -> Int
        runInstructions' arr' acc (log) acc2 =
          if (arrayIncludes (Just log) acc)
            then acc2
            else 
              if (fromMaybe emptyInstruction (index instructions' acc)).type == "jmp"
                then runInstructions' arr' (acc + ((fromMaybe emptyInstruction (index instructions' acc)).number) ) ( log <>[acc]) acc2
                else 
                  if (fromMaybe emptyInstruction (index instructions' acc)).type == "acc"
                    then runInstructions' arr' (acc + 1) (log <> [acc]) (acc2 + ((fromMaybe emptyInstruction (index instructions' acc)).number))
                    else runInstructions' arr' (acc + 1) (log <> [acc]) acc2 

    

  logShow $ runInstructions instructions'
  pure unit

