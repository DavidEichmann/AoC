{-# LANGUAGE ParallelListComp #-}

module Main where

import Safe
import qualified Data.Map as M
import Data.Maybe
import Data.List (tails)

main :: IO ()
main = aoc2b

--
-- Day 3
--

aoc3a :: IO ()
aoc3a = print $ aoc3Dist 347991

aoc3b :: IO ()
aoc3b = print $ aoc3Bound 347991

aoc3Dist :: Int -> Int
aoc3Dist i = let
    r (x, y) = (x + 1, y)
    l (x, y) = (x - 1, y)
    u (x, y) = (x, y + 1)
    d (x, y) = (x, y - 1)
    moves = concat [ replicate quartLength dir
            | quartLength' <- [1..], quartLength <- [quartLength', quartLength']
            | dir <- cycle [r,u,l,d] ]
    poses = scanl (flip ($)) (0,0) moves
    (posX, posY) = poses !! (i - 1)
    in abs posX + abs posY

aoc3Bound :: Int -> Int
aoc3Bound bound = let
    r (x, y) = (x + 1, y)
    l (x, y) = (x - 1, y)
    u (x, y) = (x, y + 1)
    d (x, y) = (x, y - 1)
    neighbors (x,y) = [(x+1,y),(x+1,y+1),(x+1,y-1),(x,y+1),(x,y-1),(x-1,y+1),(x-1,y),(x-1,y-1)]
    moves = concat [ replicate quartLength dir
            | quartLength' <- [1..], quartLength <- [quartLength', quartLength']
            | dir <- cycle [r,u,l,d] ]
    poses = scanl (flip ($)) (0,0) moves

    go :: M.Map (Int, Int) Int -> [(Int, Int)] -> Int
    go state (pos:remPoses)
      | nextValue > bound = nextValue
      | otherwise = go (M.insert pos nextValue state) remPoses
      where
        nextValue = sum . mapMaybe (`M.lookup` state) . neighbors $ pos
    in go (M.singleton (0,0) 1) (tail poses)

--
-- Day 2
--

loadAoc2 :: IO [[Int]]
loadAoc2 = do
  input <- readFile "src/input_02_a"
  return [[read word | word <- words line] | line <- lines input]

aoc2a :: IO ()
aoc2a = do
  grid <- loadAoc2
  print $ sum [maximum row - minimum row | row <- grid]

aoc2b :: IO ()
aoc2b = do
  grid <- loadAoc2
  print $ sum [ b `div` a
            | row <- grid
            , (a, b) <- take2Ordered row
            , b `rem` a == 0]
  where
    take2Ordered :: [Int] -> [(Int, Int)]
    take2Ordered xs = [(min a b, max a b) | a:bs <- tails xs, b <- bs]

--
-- Day 1
--

loadAoc1 :: IO [Int]
loadAoc1 = (mapMaybe (readMay . (:[])) <$> readFile "src/input_01_a")

aoc1a :: IO ()
aoc1a = do
  xs <- loadAoc1
  print
    . sum
    . fmap fst
    . filter (uncurry (==))
    $ zip xs (tail . cycle $ xs)

aoc1b :: IO ()
aoc1b = do
  xs <- loadAoc1
  let n = length xs
  print
    . sum
    . fmap fst
    . filter (uncurry (==))
    $ zip xs (drop (n `div` 2) . cycle $ xs)
