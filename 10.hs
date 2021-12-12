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

data ParseError = Unfinished [Char] | Mismatched Char
  deriving (Show)

dumbParse str = dumbParse' str [] where
  dumbParse' (x:xs) stack
    | x `elem` "([{<" = dumbParse' xs (x : stack)
    | x == closing (head stack) =
      mapRight (\rest -> [head stack] ++ rest)
               $ dumbParse' xs (tail stack)
    | otherwise = Left $ Mismatched x
  dumbParse' [] stack = Left $ Unfinished $ map closing stack


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

part2 = middle . sort
      . map score
      . catMaybes
      . map (\x -> case x of
          Left (Unfinished r) -> Just r
          _ -> Nothing)
      . map dumbParse where
  score = foldr (\c total -> partial c + total * 5) 0
        . reverse
  partial ')' = 1
  partial ']' = 2
  partial '}' = 3
  partial '>' = 4

  middle [x] = x
  middle  a = middle $ tail $ init a

main :: IO ()
main = interact $ wrapper . preprocess where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
