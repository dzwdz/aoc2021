import Data.List

readBase2 :: String -> Integer
readBase2 = loop . reverse where
  loop ('0' : r) = 2 * loop r
  loop ('1' : r) = 2 * loop r + 1
  loop  ""       = 0

rotate :: [[a]] -> [[a]]
rotate [a] = map (\x->[x]) a
rotate (x:xs) = concat2 (rotate [x]) (rotate xs)
  where
    concat2 a b = map (\(a,b)->a++b) $ zip a b -- TODO pointless

uniq (x:xs) = [x] ++ (filter (x /=) $ uniq xs)
uniq [] = []

-- creates a list sorted from least to most frequent
mostFrequent :: Eq a => [a] -> [a]
mostFrequent list = sortBy (\a b->compare (count a) (count b)) $ uniq list
  where
    count el = length $ filter (\x -> x == el) list

sol input = gamma * epsilon
  where
    freqs   = map mostFrequent $ rotate input
    gamma   = readBase2 $ map last freqs
    epsilon = readBase2 $ map head freqs

main :: IO ()
main = interact $ show . sol . lines
