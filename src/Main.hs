module Main where

import Data.List (unfoldr, sort)

main::IO()
main = do 
         _ <- putStrLn "enter a number"
         n <- getLine
         let r = find (read n :: Int) in
           putStrLn (show r)
         
find :: Int -> Integer
find n = let (maximum, sum, numbers) = head (firstNumbers n) in 
           let moreNumbers = (findMissingNumbers maximum sum numbers) in
              (sort moreNumbers) !! (n - 1)

firstNumbers :: Int -> [(Integer, Int, [Integer])]
firstNumbers n = snd (span (\x -> case x of (_, _, n1) -> (length n1) < n) generateNumbers)  
           
findMissingNumbers :: Integer -> Int -> [Integer] -> [Integer]
findMissingNumbers maximum sum numbers = 
  let maxSum = (truncate (log2 (fromIntegral maximum))) + 1 in 
     foldl (\seq -> \s -> 
       foldl (\seq1 -> \t -> (abcNumber t) : seq1) seq (tripletsOfSum s)
     ) numbers [sum .. maxSum]

log2 :: Float -> Float
log2 n = log n / log 2

generateNumbers :: [(Integer, Int, [Integer])]
generateNumbers = iterate generate (0, 0, [])

generate :: (Integer, Int, [Integer]) -> (Integer, Int, [Integer])
generate (max, sum, nbs) = let (newMax, newNumbers) = foldl next (max, nbs) (tripletsOfSum sum) in
                             (newMax, sum + 1, newNumbers)
                         
next :: (Integer, [Integer]) -> (Int, Int, Int) -> (Integer, [Integer])
next (max, nbs) triplet = let number = abcNumber triplet in
                            (if (number > max) then number else max, number : nbs)
  
abcNumber :: (Int, Int, Int) -> Integer  
abcNumber (a, b, c) = 2 ^ a * 3 ^ b * 5 ^ c
  
tripletsOfSum :: Int -> [(Int, Int, Int)]
tripletsOfSum n = [0 .. n] >>= (\i -> fmap (\j -> (i, j, n - i - j)) [0 .. n - i])

