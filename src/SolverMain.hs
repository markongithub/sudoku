module Main where
  import System.Environment as Environment
  import Sudoku

  main = do
    args <- Environment.getArgs
    let boardStr = head args
    let board = parseBoard boardStr
    showBoard $ solveWithGuesses board
