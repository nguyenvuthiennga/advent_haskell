module Main (main) where
import Data.Char (ord)
import Data.Set

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ans = solveProblem content
  print ans

solveProblem :: String -> Int
solveProblem s = sum (fmap (sumPriority . toList . getCommon . splitByHalf) (lines s))

splitByHalf :: String -> (String, String)
splitByHalf s = Prelude.splitAt (div (length s) 2) s

getCommon :: (String, String) -> Set Char
getCommon (x, y) = Data.Set.intersection (Data.Set.fromList x) (Data.Set.fromList y)

calPriority :: Char -> Int
calPriority c
  | (ord c >= ord 'a') = (ord c) - (ord 'a') + 1
  | otherwise =  (ord c) - (ord 'A') + 27

sumPriority :: [Char] -> Int
sumPriority s = sum (fmap calPriority s)
