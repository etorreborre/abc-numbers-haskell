module Main where

import Data.List (unfoldr, sort)

main::IO()
main = do 
         _ <- putStrLn "enter a number"
         n <- getLine
         let r = find (read n :: Int) in
           putStrLn (show r)
         
-- find the nth integer of the form 2^a*3^b*5^c (an "abc number")
--  1. find n abc numbers by generating all triplets (a, b, c) where
--     sum = a + b + c starts from 0
-- 
--  2. take the maximum of these numbers and generate more numbers by incrementing sum
--     until one of the generated numbers is >= log2(maximum). Then we know that 
--     every newly generated numbers will be superior to the nth number we are looking after
--
--  3. this means that in the list of all numbers generated so far we have the nth one
--     sort this list and return it
find :: Int -> Integer
find n = let (maximum, sum, numbers) = head (firstNumbers n) in 
           let moreNumbers = (findMissingNumbers maximum sum numbers) in
              (sort moreNumbers) !! (n - 1)

-- generate the first numbers and return
--  1. the maximum number
--  2. the current sum for a, b, c 
--  3. the list of all generated numbers
--  when the list of all generated numbers is at least of size n
firstNumbers :: Int -> [(Integer, Int, [Integer])]
firstNumbers n = snd (span (\x -> case x of (_, _, n1) -> (length n1) < n) generateNumbers)  

-- generate number, keeping track of the maximum number and the (a, b, c) sum 
generateNumbers :: [(Integer, Int, [Integer])]
generateNumbers = iterate generate (0, 0, [])

generate :: (Integer, Int, [Integer]) -> (Integer, Int, [Integer])
generate (max, sum, nbs) = let (newMax, newNumbers) = foldl next (max, nbs) (tripletsOfSum sum) in
                             (newMax, sum + 1, newNumbers)

-- next maximum number and list of numbers                         
-- for a given (a, b, c) triplet
next :: (Integer, [Integer]) -> (Int, Int, Int) -> (Integer, [Integer])
next (max, nbs) triplet = let number = abcNumber triplet in
                            (if (number > max) then number else max, number : nbs)
  

-- generate more numbers by incrementing the (a, b, c) sum to maxSum
-- where maxSum is log2(maximum number generated so far) + 1            
findMissingNumbers :: Integer -> Int -> [Integer] -> [Integer]
findMissingNumbers maximum sum numbers = 
  let maxSum = (truncate (log2 (fromIntegral maximum))) + 1 in 
     foldl (\seq -> \s -> 
       foldl (\seq1 -> \t -> (abcNumber t) : seq1) seq (tripletsOfSum s)
     ) numbers [sum .. maxSum]

-- log2
log2 :: Float -> Float
log2 n = log n / log 2

-- calculate an abc number from an (a, b, c) triplet
abcNumber :: (Int, Int, Int) -> Integer  
abcNumber (a, b, c) = 2 ^ a * 3 ^ b * 5 ^ c
  
tripletsOfSum :: Int -> [(Int, Int, Int)]
tripletsOfSum n = [(i, j, n - i - j) | i <- [0 .. n], j <- [0 .. n - i]]
  
