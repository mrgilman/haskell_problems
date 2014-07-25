import Data.Char

toDigits :: Int -> [Int]
toDigits n
    | n <= 0    = []
    | otherwise = map(\x -> digitToInt x) $ show n

doubleEveryOtherLeftToRight :: [Int] -> [Int]
doubleEveryOtherLeftToRight [] = []
doubleEveryOtherLeftToRight (x:xs) = x : (head xs * 2) : doubleEveryOtherLeftToRight(tail xs)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs = reverse $ doubleEveryOtherLeftToRight $ reverse xs

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits xs = sum . concat $ map (\x -> toDigits x) xs

validate :: Int -> Bool
validate n
    | (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0 = True
    | otherwise = False
