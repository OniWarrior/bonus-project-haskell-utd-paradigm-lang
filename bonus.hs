module Main (main) where

-- Author: Stephen Aranda
-- File  : bonus hs
-- Desc  : Bonus project for paradigm lang class
-- Date  : 3/22/23 -- completed 3/24/23

-- helper for condense and deleteAt
lengthList :: [a] -> Int -- function type
lengthList [] = 0 -- base case
lengthList (x : xs) = 1 + lengthList xs -- recursion case

-- condense: accept a list and look for consecutive elements in the list and return
-- a list with non consecutive elements.

condense :: [Integer] -> [Integer]
-- if current element is not equal to adjacent element then
-- append current element to new list and recursively call condense with tail send as arg
-- else recursively call condense with tail sent as arg.
condense (x : xs)
  | null xs = [] -- Null or empty base case
  | lengthList xs == 1 = if x /= xs !! 0 then [x, xs !! 0] else [xs !! 0] -- Base case for two elem list
  | x /= xs !! 0 = x : condense xs -- if current not eq to adj elem then append and recurive call on the tail
  | otherwise = condense xs -- otherwise recursive call with tail

-----------------------------------------------------------------------------------------------------------------

-- Helper function for deleteAt creates list of remaining elements not deleted
listConstructor :: Int -> Int -> [Integer] -> [Integer]
listConstructor count index (x : xs)
  | null xs = [] -- base case of recursion
  | count > index = if lengthList xs == 1 then [x, xs !! 0] else x : listConstructor (count + 1) index xs
  | count < index = x : listConstructor (count + 1) index xs
  | count == index = if lengthList xs == 1 then [xs !! 0] else listConstructor (count + 1) index xs

-- deleteAt: Delete an element at the index provided and create a
-- tuple that possesses the index of the deleted element and a list
-- of the remaining elements not deleted
deleteAt :: Int -> [Integer] -> (Integer, [Integer])
deleteAt index xs = (xs !! index, listConstructor 0 index xs)

--------------------------------------------------------------------------------------------------

-- findThePrimes: Helper function for find the primes
-- Recursively operate on the value provided and return a list that contains prime factors.
-- atomic is the value that will be operated on to find all prime factors
-- divisor is the value that will divide atomic
-- samePrimeFactor saves the previous factor to be used to compare to current
-- n is used to find all prime numbers that are greater than 3--result will be used to divide atomic.
findThePrimes :: Int -> Int -> Int -> Int -> [Int]
findThePrimes atomic divisor samePrimeFactor n
  | atomic == 1 = [] -- Base case that ends recursion
  | divisor == 2 -- if divisor is two check if value is evenly divisible
    =
      if mod atomic divisor == 0
        then
          if samePrimeFactor /= 2 -- if evenly divisible check if the previous factor is same as current
            then 2 : findThePrimes (div atomic divisor) divisor 2 n -- if false then append 2 to list and recursively called
            else findThePrimes (div atomic divisor) divisor samePrimeFactor n -- if not then recursively call
        else findThePrimes atomic (divisor + 1) samePrimeFactor n -- if  true that value is evenly divisble then recursively call and increment divisor by one.
  | divisor == 3 =
      if mod atomic divisor == 0 -- check for even divisible by three
        then
          if samePrimeFactor /= 3 -- check if previous factor is same as current factor
            then 3 : findThePrimes (div atomic divisor) divisor 3 n -- if false then append 3 and recursively call
            else findThePrimes (div atomic divisor) divisor samePrimeFactor n -- if true then recursively call
        else findThePrimes atomic ((6 * n) - 1) samePrimeFactor n -- if not evenly divisible then recursively call and assign next prime number as divisor
  | divisor == ((6 * n) - 1) -- check for even divisible by next prime number
    =
      if mod atomic divisor == 0
        then
          if samePrimeFactor /= ((6 * n) - 1) -- check if previous factor is same as current factor
            then ((6 * n) - 1) : findThePrimes (div atomic divisor) divisor ((6 * n) - 1) n
            else findThePrimes (div atomic divisor) divisor ((6 * n) - 1) n
        else findThePrimes atomic ((6 * n) + 1) samePrimeFactor n
  | divisor == ((6 * n) + 1) -- check for even divisible by next prime number
    =
      if mod atomic divisor == 0
        then
          if samePrimeFactor /= ((6 * n) + 1) -- check if previous factor is same as current factor
            then ((6 * n) + 1) : findThePrimes (div atomic divisor) divisor ((6 * n) + 1) n
            else findThePrimes (div atomic divisor) divisor ((6 * n) + 1) n
        else findThePrimes atomic ((6 * (n + 1)) - 1) samePrimeFactor (n + 1)

-- primeFactorList:  receives an integer input then calls findThePrimes helper function to find all prime factors of the input value
-- and return them as a list---atomic is the input variable
primeFactorList :: Int -> [Int]
primeFactorList atomic = findThePrimes atomic 2 0 1

main :: IO ()
main = do
  -- test case for two elem list with consecutive elems
  putStrLn "List to be converted [1,1]"
  print (condense [1, 1])

  -- test case for two elem list with non consecutive elems
  putStrLn "List to be converted [1,2]"
  print (condense [1, 2])

  -- test case for list with consecutive elems
  putStrLn " list to be converted [1,1,2,3]"
  print (condense [1, 1, 2, 3])

  -- test case for list of non consecutive elems
  putStrLn " List to be converted [1,2,3]"
  print (condense [1, 2, 3])

  -- test case for big list with consecutive elems
  putStrLn "List to be converted [1,1,2,2,3,4,5,5,6]"
  print (condense [1, 1, 2, 2, 3, 4, 5, 5, 6])

  -- test case for big list with non consecutive elems
  putStrLn "List to be converted [1,3,5,6,7,8,2,4]"
  print (condense [1, 3, 5, 6, 7, 8, 2, 4])

  -------------------------------------------------------

  putStrLn "Test case delete elem at index 0 of [4,5,4,3,1]"
  print (deleteAt 0 [4, 5, 4, 3, 1])

  putStrLn "Test case delete elem at index 1 of [4,5,4,3,1]"
  print (deleteAt 1 [4, 5, 4, 3, 1])

  putStrLn "Test case delete elem at index 2 of [4,5,4,3,1]"
  print (deleteAt 2 [4, 5, 4, 3, 1])

  putStrLn "Test case delete elem at index 3 of [4,5,4,3,1]"
  print (deleteAt 3 [4, 5, 4, 3, 1])

  putStrLn "Test case delete elem at index 4 of [4,5,4,3,1]"
  print (deleteAt 4 [4, 5, 4, 3, 1])

  ----------------------------------------------------------

  putStrLn "First case for primeFactorList value passed 10"
  print (primeFactorList 10)

  putStrLn "Second case for primeFactorList value passed 151"
  print (primeFactorList 151)

  putStrLn "Third case for primeFactorList value passed 213"
  print (primeFactorList 213)

  putStrLn "Fourth case for primeFactorList value passed 815"
  print (primeFactorList 815)

  putStrLn "Fifth case for primeFactorList value passed 4596"
  print (primeFactorList 4596)