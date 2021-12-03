import Data.List
import Data.Maybe

readInteger :: String -> Integer
readInteger = read

splitSingle str delim =
  fmap (\x -> splitAt x str) (elemIndex delim str)

parse line =
  (cmd, i) where
    (cmd, arg) = fromJust $ splitSingle line ' '
    i = readInteger arg


part1 = uncurry (*) . foldr step (0, 0) . map parse
  where
    step ("forward", amt) (pos, depth) = (pos + amt, depth)
    step ("down",    amt) (pos, depth) = (pos, depth + amt)
    step ("up",      amt) (pos, depth) = (pos, depth - amt)

part2 = (\(p,d,a) -> p * d) . foldr step (0, 0, 0) . map parse
  where
    step ("forward", amt) (pos, depth, aim) =
      (pos + amt, depth + aim * amt, aim)
    step ("down",    amt) (pos, depth, aim) = 
      (pos, depth, aim + amt)
    step ("up",      amt) (pos, depth, aim) =
      (pos, depth, aim - amt)


main :: IO ()
main = interact $ wrapper . lines where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
