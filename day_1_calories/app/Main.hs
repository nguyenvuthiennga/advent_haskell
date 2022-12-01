module Main (main) where
import Data.List.Split

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ans = solveProblem content
  print ans

solveProblem :: String -> (Integer, Integer)
solveProblem s = maxWithIndex (zip [0..] (sumList s))

sumList :: String -> [Integer]
sumList s = subSum (readInput s)

readInput :: String -> [[Integer]]
readInput s = map (map read) (splitOn [""] (lines s))

subSum :: [[Integer]] -> [Integer]
subSum l = map sum l

maxWithIndex :: [(Integer, Integer)] -> (Integer, Integer)
maxWithIndex [] = (-1, 0)
maxWithIndex [x] = x
maxWithIndex ((index, val):xs)
 | snd (maxWithIndex xs) > val = maxWithIndex xs
 | otherwise = (index, val)
