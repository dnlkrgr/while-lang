module Main where

import Lib
import Denotational

main :: IO ()
main = do
  print "enter file name"
  s <- getLine
  input <- readFile s
  let eitherResult = parseAndEvalProgram input
  case eitherResult of
    Left m -> print input >> print m
    Right s -> print s

