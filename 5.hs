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

arrToPair [a, b] = (a, b)

isStraight ((x1, y1), (x2, y2)) =
  x1 == x2 || y1 == y2

smartRange x y
  | x < y     = [x..y]
  | otherwise = reverse [y..x]

linePoints ((x1, y1), (x2, y2))
  | x1 == x2 = map (\y->(x1, y)) $ smartRange y1 y2
  | y1 == y2 = map (\x->(x, y1)) $ smartRange x1 x2
  | abs (x2 - x1) == abs (y2 - y1) =
      zip (smartRange x1 x2) (smartRange y1 y2)

twiceOrMore list = nub $ dubs $ sorted list where
  sorted = sortBy (comparing $ \(x,y) -> x * 10000 + y)
  dubs (x:xs@(y:_))
    | x == y    = [x] ++ dubs xs
    | otherwise = dubs xs
  dubs [_] = []
  dubs [] = []


preprocess str = map processLine $ lines str where
  processLine = arrToPair . map strToPoint . points where
    points = (\[a,_,b] -> [a, b]) . split ' '
    strToPoint = arrToPair . map readInteger . split ','

part1 = length . twiceOrMore . concat . map linePoints . filter isStraight

part2 = length . twiceOrMore . concat . map linePoints

main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
