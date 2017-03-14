module Lib
  ( simulation
  ) where

import Control.Monad
import Data.List
import System.Random (randomIO)

data Square
  = Soil
  | Seed
  | Plant
  | Rock
  | None
  deriving (Eq)

instance Show Square where
  show Soil = "."
  show Seed = "S"
  show Plant = "P"
  show Rock = "X"

data Options = Options
  { step :: Bool
  , yearsToRun :: Int
  , loadFile :: Bool
  }

fieldLength = 20

fieldWidth = 35

-- defined as a list of rows: [[fieldWidth], [fieldWidth], ...]
--                            <--------- fieldLength --------->
type Field = [[Square]]

-- TODO: improve this
replace :: Int -> Int -> a -> [[a]] -> [[a]]
replace x y newVal (c:cs)
  | x == 0 = newRow : cs
  | otherwise = c : replace (x - 1) y newVal cs
  where
    newRow = rep y c
    rep n (r:rs)
      | n == 0 = newVal : rs
      | otherwise = r : rep (n - 1) rs

getOptions :: IO Options
getOptions = do
  putStrLn
    "Welcome to the Plant Growing Simulation.\n\
    \You can step through the simulation a year at a time\n\
    \or run the simulation for 0 to 5 years.\n\
    \How many years do you want the simulation to run for?"
  years <- getLine
  putStrLn "Do you want to enable stepping mode? (Y/N)"
  step <- getLine
  putStrLn "Do you want to load a file with seed positions? (Y/N)"
  loadFile <- getLine
  return (Options (parseBool step) (read years :: Int) (parseBool loadFile))
  where
    parseBool x
      | x == "Y" || x == "y" = True
      | otherwise = False

simulation :: IO ()
simulation = do
  options <- getOptions
  field <- years (yearsToRun options)
  printField field
  where
    years n = last . take n $ iterate (>>= simulateYear) (return initialField)
    initialField = createNewField 10 17

createNewField :: Int -> Int -> Field
createNewField xpos ypos = seededSoil
  where
    soil = replicate fieldLength (replicate fieldWidth Soil)
    seededSoil = replace xpos ypos Seed soil

printField :: Field -> IO ()
printField f = putStrLn f'
  where
    f' = concatMap ((++ "\n") . concatMap show) f

-- TODO: can this be made more monadic?
simulateYear :: Field -> IO Field
simulateYear x = do
  rnd <- randomIO :: IO Int
  let (x1, frost) = simulateSpring rnd x
  when frost $ putStrLn "There has been a frost"
  printField x1
  let (x2, drought) = simulateSummer rnd x1
  when drought $ putStrLn "There has been a severe drought"
  printField x2
  let x3 = simulateAutumn x2
  printField x3
  let x4 = simulateWinter x3
  return x4

-- Seeds become plants, 50% chance of frost (kills every 3rd plant)
simulateSpring :: Int -> Field -> (Field, Bool)
simulateSpring rnd field
  | rnd `mod` 2 == 0 = (plantField field, False)
  | otherwise = (map (frost 0) (plantField field), True)
  where
    frost _ [] = []
    frost n [x]
      | x == Plant && n `mod` 2 == 1 = [Soil]
      | otherwise = [x]
    frost n (x:xs)
      | x == Plant && n `mod` 2 == 1 = Soil : frost (n + 1) xs
      | x == Plant = x : frost (n + 1) xs
      | otherwise = x : frost n xs
    plantField = map (map spring)
    spring s
      | s == Seed = Plant
      | otherwise = s

-- 50% chance of drought (every other plant becomes soil)
simulateSummer :: Int -> Field -> (Field, Bool)
simulateSummer rnd field
  | rnd `mod` 3 == 0 = (map (drought 0) field, True)
  | otherwise = (field, False)
  where
    drought _ [] = []
    drought n [Plant]
      | n `mod` 2 == 1 = [Soil]
      | otherwise = [Plant]
    drought n (Plant:xs)
      | n `mod` 2 == 1 = Soil : drought (n + 1) xs
      | otherwise = Plant : drought n xs
    drought n (x:xs) = x : drought n xs

-- Every plant drops seeds in radius 1 around it
simulateAutumn :: Field -> Field
simulateAutumn field = map autumn (columns field)
  where
    columns field = transpose $ map autumn (transpose field)
    autumn [] = []
    autumn [x] = [x]
    autumn (Soil:Plant:Soil:ys) = Seed : Plant : Seed : autumn ys
    autumn (Soil:Plant:xs) = Seed : Plant : autumn xs
    autumn (Plant:Soil:ys) = Plant : Seed : autumn ys
    autumn (x:xs) = x : autumn xs

-- Every plant dies
simulateWinter :: Field -> Field
simulateWinter = map (map winter)
  where
    winter s
      | s == Plant = Soil
      | otherwise = s
