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

zipmap :: (a -> b) -> [a] -> [(a, b)]
zipmap fn a = zip a (map fn a)

count :: Eq a => a -> [a] -> Int
count el = length . filter (\x -> x == el)


ogDigits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf",
            "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

preprocess = map processLine . lines where
  processLine line = map (map sort) $ split "|" $ words line

part1 :: [[[String]]] -> Int
part1 = length . concat . map stuff where
  stuff [_, output] = filter (\x->elem (length x) [2, 3, 4, 7]) output

part2 = sum . map translate where
  -- returns the digits which are the only ones of some length, sorted from shortest
  outliers :: [String] -> [String]
  outliers digitSet = sortBy (comparing length)
                    $ filter isOutlier digitSet where
      isOutlier digit = count (length digit) (map length digitSet) == 1

  -- returns an array of lengths of intersects with each outlier
  -- it stays stable even as the digits are switched around
  fingerprintsFrom digitSet = map fp where
    fp digit = map (length . intersect digit) $ outliers digitSet

  translate [input, output]
    = foldr (\d n -> n * 10 + d) 0 $ reverse -- convert digit list to Int
    $ map (\x -> fromJust
               $ elemIndex x        -- match them with the fingerprints of the original digits
               $ fingerprintsFrom ogDigits ogDigits)
    $ fingerprintsFrom input output -- find the fingerprints of the output digits


main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
