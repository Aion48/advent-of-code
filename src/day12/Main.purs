module Main where

import Prelude

import Data.Array (head, null, tail)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils as String.Utils
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Instruction = {
                    itype :: Char
                  , number :: Int
                    }
                  
main :: Effect Unit
main = do
  content <- readTextFile UTF8 "input.txt"
  
  
  let 
    instructions :: Array String
    instructions =  String.Utils.lines content

    convertinstr :: String -> Instruction
    convertinstr str = {
                      itype: fromMaybe '_' $ head (toCharArray str),
                      number: fromMaybe 0 $ fromString $ fromCharArray  $ fromMaybe [] $ tail (toCharArray str)
    }

    instr = map convertinstr instructions

    templateinstruction = {
                            itype: 'Q',
                            number: 0
    }
    moveferry :: Int -> Int -> Int -> Array Instruction -> Int
    moveferry northDistance eastDistance ferryFacing remainingInstructions = manhattendistance
      where 
        instruction = fromMaybe templateinstruction $ head remainingInstructions
        
        movenorth = if instruction.itype == 'N'
                      then instruction.number
                      else 
                        if instruction.itype == 'F' && ferryFacing == 0
                          then instruction.number
                          else 0
        movesouth = if instruction.itype == 'S'
                      then instruction.number
                      else 
                        if instruction.itype == 'F' && ferryFacing == 2
                          then instruction.number
                          else 0
        moveeast = if instruction.itype == 'E'
                      then instruction.number
                      else 
                        if instruction.itype == 'F' && ferryFacing == 1
                          then instruction.number
                          else 0
        movewest = if instruction.itype == 'W'
                      then instruction.number
                      else 
                        if instruction.itype == 'F' && ferryFacing == 3
                          then instruction.number
                          else 0
        
        newFacing = if instruction.itype == 'R'
                      then mod (ferryFacing + (instruction.number / 90)) 4
                      else 
                        if instruction.itype == 'L'
                          then mod (ferryFacing + ((360 - instruction.number) / 90) ) 4
                          else ferryFacing
                      

        manhattendistance = if (null remainingInstructions)
                              then (abs eastDistance) + (abs northDistance)
                              else moveferry (northDistance + (movenorth - movesouth)) (eastDistance + (moveeast - movewest)) (newFacing) (fromMaybe [] $ tail remainingInstructions)
                               



        
  {- 
  N == 0
  E == 1
  S == 2
  W == 3
  -}
  logShow $ moveferry 0 0 1 instr