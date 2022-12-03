module Main (main) where

main :: IO()
main = do
  content <- readFile "input.txt"
  print (solveProblem content)

solveProblem :: String -> Integer
solveProblem s = sum (map readLine (lines s))

readLine :: String -> Integer
readLine [l, ' ', r] = scoreLine (translateOpp l) (scoreMine r)

scoreLine :: Integer -> Integer -> Integer
scoreLine x y = (scoreGame x y) + y

scoreMine :: Char -> Integer
scoreMine s = case s of
  'X' ->  1
  'Y' ->  2
  'Z' ->  3

translateOpp :: Char -> Integer
translateOpp s = case s of
  'A' ->  1
  'B' ->  2
  'C' ->  3

scoreGame :: Integer -> Integer -> Integer
scoreGame x y
  | x == y = 3
  | (x + y) == 4 = scoreGame (mod x 3) (mod y 3)
  | (x < y) = 6
  | True = 0
