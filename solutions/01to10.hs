-- 01: Find the last element of a list
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (x:xs) = myLast xs

-- 02: Find the second-to-last element of a list
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast (x:[]) = Nothing
myButLast (x:_:[]) = Just x
myButLast (x:xs) = myButLast xs

-- 03: Find the nth element of a list
elementAt :: (Integral b) => [a] -> b -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) n
    | n < 1 = Nothing
    | n == 1 = Just x
    | otherwise = elementAt xs (n - 1)

-- 04: Find the number of elements in a list
myLength :: [a] -> Int
myLength xs = myLength' xs 0
myLength' [] n = n
myLength' (x:xs) n = myLength' xs (n + 1)

-- 05: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 06: Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs
    | length xs <= 1 = True
    | otherwise = (head xs == last xs) && (isPalindrome $ tail $ init xs)
