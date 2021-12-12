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

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight fn (Left l)  = Left l
mapRight fn (Right r) = Right $ fn r


closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'

data ParseError = Unfinished | Mismatched Char
  deriving (Show)

dumbParse str = dumbParse' str [] where
  dumbParse' (x:xs) stack
    | x `elem` "([{<" = dumbParse' xs (x : stack)
    | x == closing (head stack) =
      mapRight (\rest -> [head stack] ++ rest)
               $ dumbParse' xs (tail stack)
    | otherwise = Left $ Mismatched x
  dumbParse' [] _ = Left Unfinished


preprocess = lines
part1 = sum
      . map score
      . catMaybes
      . map (\x -> case x of
          Left (Mismatched c) -> Just c
          _ -> Nothing)
      . map dumbParse where
  score ')' = 3
  score ']' = 57
  score '}' = 1197
  score '>' = 25137

part2 _ = "TODO"

main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
