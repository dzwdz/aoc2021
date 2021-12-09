import Data.List
import Data.Maybe
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


findNeighbours :: [[a]] -> [[(a, [Maybe a])]]
findNeighbours grid = id
                    $ transpose
                    $ map lineNeighbours
                    $ transpose
                    $ map lineNeighbours
                    $ addArrays grid where

  addArrays grid = map (map (\x -> (x, []))) grid
  lineNeighbours line = id
                      $ map (\((ch, arr), (right, left)) -> (ch, arr ++ [left, right]))
                      $ zip line
                      $ zip (tail $ map (Just . fst) line ++ [Nothing])
                            ([Nothing] ++ map (Just . fst) line)

preprocess = map (map (\c -> readInteger [c])) . lines
part1 grid = sum
           $ map succ
           $ map fst
           $ filter (\(h, nears) -> h < (minimum $ catMaybes nears))
           $ concat
           $ findNeighbours grid
part2 _ = "TODO"

main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
