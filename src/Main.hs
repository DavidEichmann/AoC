module Main where

import Safe
import Data.Maybe

main :: IO ()
main = do
  xs <- (mapMaybe (readMay . (:[])) <$> readFile "src/input") :: IO [Integer]
  print xs
  print
    . sum
    . fmap fst
    . filter (uncurry (==))
    $ zip xs (tail . cycle $ xs)
