-- Задача 2
gcd' :: Int -> Int -> Int 
gcd' a 0 = abs a
gcd' a b = gcd' b (mod a b)

-- Задача 3
isPrime :: Int -> Bool
isPrime n = not (testSimple n 2)
testSimple :: Int -> Int -> Bool
testSimple 1 x = True
testSimple 2 x = False
testSimple n x = ((x * x) <= n) && (((mod n x) == 0) || testSimple n (x + 1))

-- Задача 4
reverseNumber :: Integer -> Integer
reverseNumber x = calcReverse x 0
calcReverse :: Integer -> Integer -> Integer
calcReverse x acc = if (x == 0) 
 then acc
 else calcReverse (div x 10) ((acc * 10) + (mod x 10))

-- Задача 5
maxRoot :: Int -> Int -> Int -> Double
maxRoot a b c = 
 let d = discriminant a b c;
 in if (d == (0/0))
  then 0/0
  else max (calcResPolinom a b d) (calcResPolinom a b (-d))

calcResPolinom:: Int -> Int -> Double -> Double
calcResPolinom a b d = (d - (fromIntegral b)) / ((fromIntegral a) * 2)

discriminant :: Int -> Int -> Int -> Double
discriminant a b c = if ((b * b) - (4 * (a * c)) >= 0) 
 then sqrt (fromIntegral ((b * b) - (4 * (a * c))))
 else 0/0


funcTest :: Double -> Double
funcTest x = (x * x) - 1

 -- Задача 6
root :: (Double->Double) -> Double -> Double -> Double -> Double
root f a b eps = rootCalc (f) a b eps ((b + a) / 2) 

rootCalc :: (Double->Double) -> Double -> Double -> Double -> Double -> Double
rootCalc f a b eps c = if (abs (a - b)) < eps 
 then c 
 else if ((f a) * (f c)) < 0
  then rootCalc f a c eps ((c + a) / 2)
  else rootCalc f c b eps ((b + c) / 2)