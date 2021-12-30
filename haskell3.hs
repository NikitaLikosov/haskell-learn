{-# LANGUAGE MultiWayIf #-}  --Подключаем расширения языка Haskell 
import Data.Char  --Подключаем расширения языка Haskell Data.Char 
import Data.List

-- Задача 1.1 num2lst 123
num2lst :: Integer -> [Int]
num2lst x = num2lstCalc x []

num2lstCalc :: Integer -> [Int] -> [Int]
num2lstCalc x lst  
    | div x 10 > 0 = num2lstCalc (div x 10) ((fromInteger (mod x 10)) : lst) 
    | otherwise = (fromInteger x) : lst

-- Задача 1.2 lst2num [1, 2, 3]
lst2num :: [Int] -> Integer
lst2num lst = foldl lst2numCalc 0 lst 

lst2numCalc :: Integer -> Int -> Integer
lst2numCalc x y = (x * 10) + (fromIntegral y)

-- Задача 2.1 lst2num [1, 2, 3]
sumSquareLst1 :: [Int] -> Integer
sumSquareLst1 lst 
    | null lst = 0 
    | otherwise = (sumSquareLst1 (tail lst)) + (fromIntegral ((head  lst) * (head  lst)))  

-- Задача 2.2 lst2num [1, 2, 3]
sumSquareLst2 :: [Int] -> Integer
sumSquareLst2 lst = sumSquareLstCalc2 lst 0

sumSquareLstCalc2 :: [Int] -> Integer -> Integer
sumSquareLstCalc2 lst acc 
    | null lst = acc 
    | otherwise = (sumSquareLstCalc2 (tail lst) (toInteger ((head  lst) * (head  lst))) + acc)

-- Задача 2.3 lst2num [1, 2, 3]
sumSquareLst3 :: [Int] -> Integer
sumSquareLst3 lst = foldl sumSquareLstCalc3 0 lst

sumSquareLstCalc3 :: Integer -> Int -> Integer
sumSquareLstCalc3 x y = (fromIntegral (y * y)) + x 

test2 :: [Int] -> Bool
test2 lst = ((sumSquareLst1 lst) == (sumSquareLst2 lst)) && ((sumSquareLst2 lst) == (sumSquareLst3 lst))


posMax :: [String] -> String
posMax mat
   | (null (head mat)) = []
   | otherwise = (foldl posMaxCalc (chr 0) (map head mat)) : (posMax (map tail mat))

posMaxCalc :: Char -> Char -> Char 
posMaxCalc acc c
  | (ord acc) >= (ord c) = acc
  | otherwise = c

f :: [String] -> String
f = foldl1 (zipWith max)