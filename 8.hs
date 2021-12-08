import Data.List
import Data.Maybe
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
  processLine line = map (map sort) $ split "|" $ words line

part1 patterns = length $ concat $ map stuff patterns where
  stuff [_, output] = filter (\x->elem (length x) [2, 3, 4, 7]) output

part2 patterns = sum $ map translate patterns where
  translate [input, output] = asNumber where
    ofLength len = filter (\a -> length a == len) input

    d1 = head $ ofLength 2
    d7 = head $ ofLength 3
    d4 = head $ ofLength 4
    d8 = head $ ofLength 7
    d3 = head
          $ filter (\x -> intersect x d1 == d1) -- d3 
          $ ofLength 5                          -- d2, d3, d5
    [d2, d5] = sortBy (comparing
                            (length . intersect d4)) -- [d2 (len 2), d5 (len 3)]
                   $ filter (/= d3) -- d2 ; d5
                   $ ofLength 5     -- d2 ; d3 ; d5
    d9 = head
           $ filter (\x -> intersect x d4 == d4)
           $ ofLength 6
    [d6, d0] = sortBy (comparing
               (length . intersect d1))
      $ filter (/= d9)
      $ ofLength 6
    digitList = [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9]

    digits = map (\d -> fromJust $ elemIndex d digitList) output
    asNumber = foldr (\d n -> n * 10 + d) 0 $ reverse digits

main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
