-- TODO: Import encode from first problem set
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr reducer []
    where reducer x [] = [(x, 1)]
          reducer x (freq:freqs)
            | x == (fst freq) = ((x, (snd freq + 1)):freqs)
            | otherwise = (x, 1):freq:freqs


data FreqRecord a = Single a | Multiple (a, Int) deriving (Eq, Show)

encodeModified x = parse $ encode x
    where parse :: Eq a => [(a, Int)] -> [FreqRecord a]
          parse [] = []
          parse ((x, 1):xs) = Single x : parse xs
          parse ((x, y):xs) = Multiple (x, y) : parse xs

decodeModified :: [FreqRecord a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = [x] ++ decodeModified xs
decodeModified (Multiple (x, y):xs) = take y (repeat x) ++ decodeModified xs

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = take n (repeat x) ++ repli xs n

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst [(xs !! i, i) | i <- [0..length xs - 1], i `mod` n == 0]

split :: [a] -> Int -> [[a]]
split xs n = [take n xs] ++ [drop n xs]

slice :: [a] -> Int -> Int -> [a]
slice xs x y = take (y - x + 1) (drop (x - 1) xs)

rotate :: [a] -> Int -> [a]
rotate (x:xs) n
    | n < 0 = rotate (x:xs) (length xs - n)
    | n > 0 = rotate (xs ++ [x])  (n - 1)
    | otherwise = x:xs

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), take (n -  1) xs ++ drop n xs)

main = print $ removeAt 2 ['a','b','c','d']
