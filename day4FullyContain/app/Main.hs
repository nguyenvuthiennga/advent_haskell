module Main (main) where
import Data.List.Split


main :: IO ()
main = do
  content <- readFile "input.txt"
  let ans = solveProblem content
  print ans

solveProblem :: String -> Int
solveProblem s = length (filter (\n -> n == True) (fmap fullyContain (parseInput s)))

parseInput :: String -> [((Integer, Integer), (Integer, Integer))]
parseInput s = map parseLine (lines s)

parseLine :: String -> ((Integer, Integer), (Integer, Integer))
parseLine s =  getRanges (splitOn "," s)

getRanges :: [String] -> ((Integer, Integer), (Integer, Integer))
getRanges (f:s:_) = ((parseRange f), (parseRange s))

parseRange :: String -> (Integer, Integer)
parseRange s = getRange (splitOn "-" s)

getRange :: [String] -> (Integer, Integer)
getRange (f:s:_) = (read f, read s)

calRange :: (Integer, Integer) -> (Double, Double)
calRange (l, u) = (fromIntegral (l + u) / 2, fromIntegral (u - l) / 2)

fullyContain :: ((Integer, Integer), (Integer, Integer)) -> Bool
fullyContain ((l1, u1), (l2, u2)) = fullyContainRadius (calRange (l1, u1), calRange (l2, u2))

fullyContainRadius :: ((Double, Double), (Double, Double)) -> Bool
fullyContainRadius ((c1, r1), (c2, r2)) = (abs (c1 - c2)) <= (abs (r1 - r2))
