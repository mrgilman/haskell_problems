import Data.Char

toDigits :: Int -> [Int]
toDigits n
    | n <= 0    = []
    | otherwise = map digitToInt $ show n

doubleEveryOtherLeftToRight :: [Int] -> [Int]
doubleEveryOtherLeftToRight [] = []
doubleEveryOtherLeftToRight [x] = [x]
doubleEveryOtherLeftToRight (x:y:xs) = x : y * 2 : doubleEveryOtherLeftToRight xs

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = reverse . doubleEveryOtherLeftToRight . reverse

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits xs = sum . concat $ map toDigits xs

validate :: Int -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0
