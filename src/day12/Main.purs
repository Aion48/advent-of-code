module Main where

import Prelude

import Data.Array (cons, head, null, tail)
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

    movewaypoint :: Int -> Int -> Int -> Int -> Array Instruction -> Int                        
    movewaypoint shipn shipe waypointn waypointe remainingInstructions = manhattendistance
      where
        instruction = fromMaybe templateinstruction $ head remainingInstructions

        movenorth = if instruction.itype == 'N'
                      then instruction.number
                      else 0
        movesouth = if instruction.itype == 'S'
                      then instruction.number
                      else 0
        moveeast = if instruction.itype == 'E'
                      then instruction.number
                      else 0
        movewest = if instruction.itype == 'W'
                      then instruction.number
                      else 0

        newwaypointNorth = if instruction.itype == 'L'
                            then waypointe
                            else 
                              if instruction.itype == 'R' 
                                then (0 - waypointe)
                                else waypointn + (movenorth - movesouth)
        newwaypointEast = if instruction.itype == 'L'
                            then (0 - waypointn)
                            else 
                              if instruction.itype == 'R' 
                                then waypointn
                                else waypointe + (moveeast - movewest)

        towardwaypointn = if instruction.itype == 'F'
                            then shipn + (instruction.number * waypointn)
                            else shipn
        towardwaypointe = if instruction.itype == 'F'
                            then shipe + (instruction.number * waypointe)
                            else shipe
                              

        manhattendistance = if (null remainingInstructions)
                              then (abs shipn) + (abs shipe)
                              else 
                                if (instruction.itype == 'R') && (instruction.number > 90)
                                  then movewaypoint towardwaypointn towardwaypointe newwaypointNorth newwaypointEast (cons ({itype: 'R',number: (instruction.number - 90)}) (fromMaybe [] $ tail remainingInstructions))
                                  else if (instruction.itype == 'L') && (instruction.number > 90)
                                    then movewaypoint towardwaypointn towardwaypointe newwaypointNorth newwaypointEast (cons ({itype: 'L',number: (instruction.number - 90)}) (fromMaybe [] $ tail remainingInstructions))
                                    else movewaypoint towardwaypointn towardwaypointe newwaypointNorth newwaypointEast (fromMaybe [] $ tail remainingInstructions)
        


        
  {- 
  N == 0
  E == 1
  S == 2
  W == 3
  -}
  logShow $ movewaypoint 0 0 1 10 instr