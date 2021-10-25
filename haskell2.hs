{-# LANGUAGE MultiWayIf #-}  --Подключаем расширения языка Haskell 

-- Задача 1 dFact 6
dFact :: Integer -> Integer
dFact x = 
 if | x >= 1 -> x * dFact (x - 2)
    | otherwise -> 1

-- Задача 2 sumOfDigits 123
sumOfDigits :: Integer -> Int
sumOfDigits x = fromInteger (sumOfDigitsCalc x)

sumOfDigitsCalc :: Integer -> Integer
sumOfDigitsCalc x 
    | div x 10 > 0 = mod x 10 + sumOfDigitsCalc (div x 10) 
    | otherwise = x   

-- Задача 3 powOf2 1
powOf2 :: Integer -> Int
powOf2 x = powOf2Calc x 0

powOf2Calc :: Integer -> Int -> Int 
--powOf2Calc 1 n = n
powOf2Calc x n
    | x == 1 = n
    | mod x 2 == 0 = powOf2Calc (div x 2) (n + 1)
    | otherwise = -1

-- Задача 4 qntPoints 0 0 3 0 0 3  
qntPoints :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Int
qntPoints x1 y1 r1 x2 y2 r2 = 
 let distance = distanceCalc x1 y1 x2 y2
 in if | x1 == x2 && y1 == y2 && r1 == r2 -> 3
       | distance >  r1' + r2' -> 0
       | distance == r1' + r2' -> 1
       | distance < r1' && distance + r2' == r1' -> 1
       | distance < r2' && distance + r1' == r2' -> 1
       | distance < r1' && distance + r2' > r1' -> 2
       | distance < r2' && distance + r1' > r2' -> 2
       | otherwise -> 2
       where r1' = fromIntegral r1
             r2' = fromIntegral r2

distanceCalc :: Integer -> Integer -> Integer -> Integer -> Double
distanceCalc x1 y1 x2 y2 = sqrt ((fromIntegral((x2 - x1) ^ 2)) + (fromIntegral ((y2 - y1) ^ 2)))


-- Задача 5
mySin :: Double -> Double -> Double
mySin a eps =
 let x = ezSin a
 in calcMySin x eps 1 1

calcMySin :: Double -> Double -> Int -> Double -> Double
calcMySin x eps n sign =
 let result = ((x ^ n) / (fromIntegral (fact n)))
 in 
  if | result < eps -> 0
     | otherwise -> ((result * sign) + (calcMySin x eps (n + 2) (- sign)))                  

ezSin :: Double -> Double
ezSin x = x - ((fromIntegral (floor (x / (2.0 * pi)))) * (2.0 * pi))

fact :: Int -> Int 
fact x = 
 if | x == 1 -> 1
    | otherwise -> x * (fact (x - 1))

-- Задача 6
 
-- exerciseA_1 :: Integer -> Integer -> Integer
-- exerciseA_1 x y = x ^ y

-- exerciseA_2 = (`exerciseA_1` )