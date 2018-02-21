
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls 
    | even $ length ls = double ls
    | otherwise        = skip ls
    where
    double :: [Integer] -> [Integer]
    double []     = []
    double (x:xs) = (2*x) : skip xs
    skip :: [Integer] -> [Integer]
    skip []       = []
    skip (x:xs)   = x : double xs

sumDigits :: [Integer] -> Integer
sumDigits ls = sum $ concat $ map toDigits ls

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

