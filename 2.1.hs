import Data.List
import Data.Maybe

readInteger :: String -> Integer
readInteger = read

applyToTuple fn (a, b) = fn a b -- TODO is that in the stdlib?

splitSingle str delim =
  fmap (\x -> splitAt x str) (elemIndex delim str)

parse line =
  (cmd, i) where
    (cmd, arg) = fromJust $ splitSingle line ' '
    i = readInteger arg

sol = applyToTuple (*) . foldl step (0, 0) . map parse
  where
    step (pos, depth) ("forward", amt) = (pos + amt, depth)
    step (pos, depth) ("down",    amt) = (pos, depth + amt)
    step (pos, depth) ("up",      amt) = (pos, depth - amt)

main :: IO ()
main = interact $ show . sol . lines
