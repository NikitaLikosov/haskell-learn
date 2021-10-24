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


 maxRoot:: Int -> Int -> Int -> Double 
 maxRoot a b c = sqrt (fromIntegral ((b * b) - (4 * (a * c))))