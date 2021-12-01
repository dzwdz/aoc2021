readInteger s = read s :: Integer

pairs :: Num a => [a] -> [(a, a)]
pairs (x:y:xs) = [(x, y)] ++ pairs ([y] ++ xs)
pairs [x] = []
pairs [] = []

pairs' a = zip a (tail a) -- unsafe but oh well

sol lines = length $ filter (\(x,y) -> x < y) $ pairs' $ map readInteger lines

main = interact $ show . sol . lines
