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

findMatches :: Eq a => [a] -> [[a]] -> [[Bool]]
findMatches nums =
  map (map (\x->elem x nums))

checkStraights :: [[Bool]] -> Bool
checkStraights board = row || column where
  match  = [True, True, True, True, True]
  row    = elem match board
  column = elem match $ transpose board

partialScore nums board = sum $ filter (\x->not $ elem x nums) $ concat board

boardInfo nums board = (score, time) where
  scores = map score (inits nums) where
    score nums =
      if (checkStraights $ findMatches nums board)
        then Just (partialScore nums board * last nums)
        else Nothing

  score = head $ catMaybes scores
  time = length $ filter (Nothing ==) scores


preprocess str = (nums, boards) where
  (nums':boards') = split "" $ lines str
  nums = map readInteger $ split ',' $ head nums'
  boards = map (map (map readInteger . filter ("" /=) . split ' ')) boards'

part1 (nums, boards) =
  minimumBy (comparing snd) $ map (boardInfo nums) boards

part2 (nums, boards) =
  maximumBy (comparing snd) $ map (boardInfo nums) boards


main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
