import Data.List
import Data.Ord
import Debug.Trace

coolTrace a = trace (show a) a

readBase2 :: String -> Integer
readBase2 = loop . reverse where
  loop ('0' : r) = 2 * loop r
  loop ('1' : r) = 2 * loop r + 1
  loop  ""       = 0

rotate :: [[a]] -> [[a]]
rotate [] = []
rotate [a] = map (\x->[x]) a
rotate (x:xs) = concat2 (rotate [x]) (rotate xs)
  where
    concat2 a b = map (uncurry (++)) $ zip a b -- TODO why can't i make this pointless?

uniq :: Eq a => [a] -> [a]
uniq (x:xs) = [x] ++ (filter (x /=) $ uniq xs)
uniq [] = []

mostFrequent :: Eq a => [a] -> a
mostFrequent list = maximumBy (comparing count) $ uniq list
  where  count el = length $ filter (\x -> x == el) list

leastFrequent :: Eq a => [a] -> a
leastFrequent list = minimumBy (comparing count) $ uniq list
  where  count el = length $ filter (\x -> x == el) list


part1 input = gamma * epsilon
  where
    freqs   = map mostFrequent $ rotate input
    gamma   = readBase2 $ map mostFrequent $ rotate input
    epsilon = readBase2 $ map leastFrequent $ rotate input


rate determinant list = rate' $ zip list (map readBase2 list)
  where
    rate' [(_, og)] = og
    rate' list = rate' $ map (\(x,og) -> (tail x, og)) matching
      where
        dumbWeight x = x ++ x ++ ['1'] -- in case of a tie, 1 is supposed to be the most common bit

        targetBit = determinant $ dumbWeight $ map (head . fst) list
        matching = filter ((targetBit ==) . head . fst) list

part2 input = oxyRating * co2Rating where
  oxyRating = rate  mostFrequent input
  co2Rating = rate leastFrequent input


main :: IO ()
main = interact $ wrapper . lines where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
