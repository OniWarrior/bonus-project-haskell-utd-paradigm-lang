module Main (main) where

-- Author: Stephen Aranda
-- File  : bonus hs
-- Desc  : Bonus project for paradigm lang class
-- Date  : 3/22/23

-- condense: accept a list and look for consecutive elements in the list and return
-- a list with non consecutive elements.

-- helper for condense
lengthList :: [a] -> Int -- function type
lengthList [] = 0 -- base case
lengthList (x : xs) = 1 + lengthList xs -- recursion case

condense :: [Integer] -> [Integer]
-- if current element is not equal to adjacent element then
-- append current element to new list and recursively call condense with tail send as arg
-- else recursively call condense with tail sent as arg.
condense (x : xs)
  | null xs = [] -- Null or empty base case
  | lengthList xs == 1 = if x /= xs !! 0 then [x, xs !! 0] else [xs !! 0] -- Base case for two elem list
  | x /= xs !! 0 = x : condense xs -- if current not eq to adj elem then append and recurive call on the tail
  | otherwise = condense xs -- otherwise recursive call with tail

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