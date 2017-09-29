module HW01 where

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = [n `mod` 10] ++ toDigitsRev (n `div` 10)

helpDouble :: [Integer] -> [Integer]
helpDouble [] = []
helpDouble (x:[]) = [x]
helpDouble (x) = reverse(doubleEveryOther(reverse(x)))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) =
    x : (y*2) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:ys) =
  -- CM: see comment in pull request
  (x `div` 10) + (x `mod` 10) + sumDigits ys

validate :: Integer -> Bool
validate n =
  (sumDigits(helpDouble(toDigits(n))) `mod` 10) == 0
