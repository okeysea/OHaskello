module MyMath where

isPrime :: Integral a => a -> Bool
isPrime n
  | n         <= 1 = False
  | n         == 2 = True
  | n `mod` 2 == 0 = False
  | otherwise      = do
    length ( filter (\x -> n `mod` x == 0) (oddList 3 ( ceiling (sqrt $ fromIntegral n )))) == 0

oddList :: Integral a => a -> a -> [a]
oddList m n = [x | x <- [m..n], odd x]

primeList :: Integral a => a -> [a]
primeList n = filter (\x -> isPrime(x)) [1..n]

-- fizzbuzz
mkList :: [String]
mkList = map convert list
  where
    list = [1..30] :: [Int]
    convert :: Int -> String
    convert n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod`  3 == 0 = "Fizz"
      | n `mod`  5 == 0 = "Buzz"
      | otherwise       = show n

doMain = do
  print $ isPrime 99991
  print $ primeList 100000
  -- writeFile "prime_numbers.txt" $ show $ primeList 1000000
