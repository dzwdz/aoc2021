readInteger :: String -> Integer
readInteger = read

pairs :: Num a => [a] -> [(a, a)]
pairs (x:y:xs) = [(x, y)] ++ pairs ([y] ++ xs)
pairs [_] = []
pairs [] = []

pairs' :: Num a => [a] -> [(a, a)]
pairs' a = zip a (tail a) -- unsafe but oh well

sol :: [String] -> Int
sol = length . filter (\(x,y) -> x < y) . pairs' . map readInteger 

main :: IO ()
main = interact $ show . sol . lines
