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

elemCount :: Eq a => a -> [a] -> Int
elemCount _ [] = 0
elemCount needle (x:xs)
  | x == needle = 1 + elemCount needle xs
  | otherwise   =     elemCount needle xs

setAt :: Int -> a -> [a] -> [a]
setAt pos val arr = take pos arr ++ [val] ++ drop (succ pos)  arr

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt pos fn arr = setAt pos (fn $ arr !! pos) arr

nTimes :: Int -> (a -> a) -> a -> a
nTimes times fn
  | times > 0  = nTimes (pred times) fn . fn
  | times == 0 = id


step counts = res where
  rotated = tail counts ++ [head counts]
  res = modifyAt 6 (+ head counts) rotated


preprocess input = counts where
  fish = map readInteger $ split ',' input
  counts = map (\i -> elemCount i fish) [0..8]

part1 input = sum $ nTimes 80 step input
part2 input = sum $ nTimes 256 step input

main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
