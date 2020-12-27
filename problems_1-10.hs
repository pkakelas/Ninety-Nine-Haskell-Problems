myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = head . tail . reverse

elementAt :: Num a => [a] -> Int -> a
elementAt x n = x !! n - 1

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs)
  | x `elem` xs = compress xs
  | otherwise = [x] ++ compress xs 

pack :: Eq a => [a] -> [[a]]
pack = foldr reducer []
    where reducer x [] = [[x]]
          reducer x (pack:packs)
            | x == (head pack) = ((x:pack):packs)
            | otherwise = [x]:pack:packs

encode :: Eq a => [a] -> [(a, Int)]
encode = foldr reducer []
    where reducer x [] = [(x, 1)]
          reducer x (freq:freqs)
            | x == (fst freq) = ((x, (snd freq + 1)):freqs)
            | otherwise = (x, 1):freq:freqs
