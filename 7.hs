import Data.List
import Data.Ord
import Debug.Trace
coolTrace a = trace (show a) a

readInteger :: String -> Integer
readInteger = read

split :: Eq a => a -> [a] -> [[a]]
split delim arr =
  case elemIndex delim arr of
    Nothing  -> [arr]
    Just idx -> result where
      (pre, post') = splitAt idx arr
      post = tail post'
      result = [pre] ++ split delim post

cost crabs fn target = sum $ map (\x -> fn $ abs $ target - x) crabs

sumUntil x = sum [1..x]
sumUntil' x = x * (x + 1) `div` 2


preprocess = map readInteger . split ','
part1 crabs = minimum $ map (cost crabs id) (nub crabs)

part2 crabs = minimum $ map (cost crabs sumUntil') range
  where
    min = minimum crabs
    max = maximum crabs
    range = [min..max]

main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
