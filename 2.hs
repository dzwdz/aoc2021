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

part1 = applyToTuple (*) . foldl step (0, 0) . map parse
  where
    step (pos, depth) ("forward", amt) = (pos + amt, depth)
    step (pos, depth) ("down",    amt) = (pos, depth + amt)
    step (pos, depth) ("up",      amt) = (pos, depth - amt)

part2 = (\(p,d,a) -> p * d) . foldl step (0, 0, 0) . map parse
  where
    step (pos, depth, aim) ("forward", amt) =
      (pos + amt, depth + aim * amt, aim)
    step (pos, depth, aim) ("down",    amt) =
      (pos, depth, aim + amt)
    step (pos, depth, aim) ("up",      amt) =
      (pos, depth, aim - amt)


main :: IO ()
main = interact $ wrapper . lines where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
