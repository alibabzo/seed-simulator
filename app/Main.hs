module Main where

import Simulation
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parse >>= simulation

parse :: [String] -> IO Options
parse [years] = return $ Options Nothing (read years)
parse [years, "-f", file] = do
  contents <- readFile file
  return $ Options (Just contents) (read years)

parse _ = errorWithoutStackTrace "Usage: seed-simulator <years to run> [-f <file>]"
