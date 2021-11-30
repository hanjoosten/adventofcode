module Main2021 (getPuzzle) where

import Data.List (intercalate, intersperse, nub, partition, sort)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShowId)
import System.FilePath (dropExtension,(</>))
import Text.Read (Lexeme (String))



getPuzzle :: ([String] -> String, FilePath)
getPuzzle = from  puzzle24
   where from (a,b) = (a,show 2021 </> b) 

puzzle24 :: ([String] -> String, FilePath)
puzzle24 = (fun, "puzzle_1.txt")
  where
    fun rows = "Klaar voor de eerste puzzel"

