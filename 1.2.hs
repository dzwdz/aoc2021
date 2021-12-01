readInteger :: String -> Integer
readInteger = read

pairs' :: Num a => [a] -> [(a, a)]
pairs' a = zip a (tail a)

window :: Integer -> [[a]] -> [[a]]
window 1 a = a
window size a = map concat $ zip a (window (size - 1) $ tail a)
windowSums size = map sum . window size . map (\x->[x])

sol = length . filter (\(x,y) -> x < y) . pairs' . windowSums 3 . map readInteger 

main :: IO ()
main = interact $ show . sol . lines
