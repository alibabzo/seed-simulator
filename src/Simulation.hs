module Simulation
  ( Options (Options)
  , simulation
  , stepSimulation
  ) where

import Control.Monad.State.Strict
import Data.Array
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, isNothing)
import System.Random (randomIO)

data Square = Soil | Seed | Plant | Rock
  deriving Eq

instance Show Square where
  show Soil = "."
  show Seed = "S"
  show Plant = "P"
  show Rock = "X"

read' :: Char -> Square
read' char = case char of
  'S' -> Seed
  'P' -> Plant
  'X' -> Rock
  _ -> Soil

data Options = Options
  { fileContents :: Maybe String
  , yearsToRun :: Int
  }

type Field = Array (Int, Int) Square

-- ypos          xpos
fieldLength, fieldWidth :: Int
fieldLength = 20
fieldWidth = 35

initialField :: Options -> Field
initialField options | isNothing $ fileContents options = defaultField
                     | otherwise = parseFile $ fromMaybe "" (fileContents options)
                     where
                       defaultField = createNewField (18, 10)

parseFile :: String -> Field
parseFile = arr . flatten
  where
    arr = array ((1, 1), (fieldLength, fieldWidth)) . zip indexes
    flatten = map read' . concat . lines
    indexes = [(x, y) | x <- [1 .. fieldLength], y <- [1 .. fieldWidth]]

createNewField :: (Int, Int) -> Field
createNewField (x, y) = soil // [((y, x), Seed)]
  where soil = array ((1, 1), (fieldLength, fieldWidth)) [((y, x), Soil) | y <- [1..fieldLength], x <- [1..fieldWidth]]


printField :: Field -> IO ()
printField = putStrLn . unlines . map (concatMap show) . chunksOf fieldWidth . elems

stepSimulation :: Options -> IO ()
stepSimulation options = undefined

simulation :: Options -> IO ()
simulation options = go (yearsToRun options) (initialField options)
  where go 0 _ = return ()
        go n f = do
          field <- execStateT simulateYear f
          go (n - 1) field


simulateYear :: StateT Field IO Field
simulateYear = do
  simulateSpring
  simulateSummer
  simulateAutumn
  simulateWinter

simulateSpring :: StateT Field IO Field
simulateSpring = do
  field <- get
  frost <- io (randomIO :: IO Bool)
  if frost then do
    io $ putStrLn "frost"
    execute (changeSquares Seed Soil 3) field
  else
    execute (changeSquares Seed Plant 1) field

simulateSummer :: StateT Field IO Field
simulateSummer = do
  field <- get
  drought <- io (randomIO :: IO Bool)
  if drought then do
     io $ putStrLn "drought"
     execute (changeSquares Plant Soil 2) field
  else do
    io $ printField field
    return field


simulateAutumn :: StateT Field IO Field
simulateAutumn = do
  field <- get
  execute (\x -> x // replacements x) field
  where replacements field = concatMap (seed field) [(y, x) | y <- [1..fieldLength], x <- [1..fieldWidth], field ! (y, x) == Plant]
        seed field (y, x) = [((a, b), Seed) | a <- [y - 1 .. y + 1], b <- [x - 1 .. x + 1]
                      , 1 <= a && a <= fieldLength
                      , 1 <= b && b <= fieldWidth
                      , field ! (a, b) == Soil
                      ]

simulateWinter :: StateT Field IO Field
simulateWinter = do
  field <- get
  execute (changeSquares Plant Soil 1) field

changeSquares :: Square -> Square -> Int -> Field -> Field
changeSquares s0 s1 n f = f // each n [((y, x), s1) | y <- [1..fieldLength], x <- [1..fieldWidth]
                                      , f ! (y, x) == s0
                                      ]
  where each 1 l = l
        each n l | length l > n = map last . chunksOf n $ l
                 | otherwise = []

io :: IO a -> StateT Field IO a
io = liftIO

execute :: (Field -> Field) -> Field -> StateT Field IO Field
execute f x = do
  put (f x)
  io $ printField x
  return x
