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


preprocess = map processLine . lines where
  processLine line = split "|" $ words line

part1 patterns = length $ concat $ map stuff patterns where
  stuff [_, output] = filter (\x->elem (length x) [2, 3, 4, 7]) output

part2 _ = "TODO"

main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
