import Data.List
import Data.Ord

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

part2 input = "TODO"


main :: IO ()
main = interact $ wrapper . lines where
  wrapper arg = "part 1:\n" ++ (show $ part1 arg)
         ++ "\n\npart 2:\n" ++ (show $ part2 arg)
