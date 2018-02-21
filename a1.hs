
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFromLeft . reverse
    where
    doubleFromLeft :: [Integer] -> [Integer]
    doubleFromLeft []       = []
    doubleFromLeft [x]      = [2 * x]
    doubleFromLeft (x:y:xs) = x : (2 * y) : doubleFromLeft xs

sumDigits :: [Integer] -> Integer
sumDigits ls = sum $ concat $ map toDigits ls

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0
