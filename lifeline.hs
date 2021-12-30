{-# LANGUAGE MultiWayIf #-}  --Подключаем расширения языка Haskell 
import Data.Char  --Подключаем расширения языка Haskell Data.Char  (reverse (dropWhile (<m) (reverse (dropWhile (< m) list))
import Data.List
import Data.Ord

slice :: Ord a => [a] -> [a]
slice l = let m = (maximum l)
  in if m == maximum (delete (m) l)  
      then (reverse (dropWhile (<m) (reverse (dropWhile (<m) l))))
      else []



-- sumDivision x 2 
sumDivisionCalc :: Integer -> Integer -> Integer
sumDivisionCalc n x = if ((n^2) > x)
  then 0
  else if ((mod x n) == 0) 
    then if (x `div` n) ^2 <= x 
      then (n + ( sumDivisionCalc (n + 1) x))
      else (n + (div x n) + ( sumDivisionCalc (n + 1) x))
    else sumDivisionCalc (n + 1) x 

sumDivision :: Integer -> Integer
sumDivision x = 1 + (sumDivisionCalc 2 x)

friendNumbers :: Integer -> [[Integer]]
friendNumbers x =
  if x == 0 
    then []
    else 
      let y = sumDivision x
      in 
        if y < x && sumDivision y == x
          then [x, y] : friendNumbers (x - 1) 
          else friendNumbers (x - 1)

-- newtype Poly = Poly [Integer] deriving (Show, Eq)

-- degre (Poly p) = (length p)-1
 
-- value (Poly p) x = foldl (\ acc v -> acc*x+v) 0 p

-- derivative (Poly p) = reverse $ delete 0 (zipWith (*) (reverse p) [0..(length p)])

-- calcMultiplication (Poly p) (Poly q) n = (map (* hp) q)++(take n (repeat 0))
--                                                     where (hp : tp) = p

-- instance  Num Poly where
  
--   negate (Poly p) = (Poly $ map negate p)
  
--   (+) (Poly p) (Poly q) = (Poly $ zipWith (+) pp qq)
--                           where pp=(take pk (repeat 0))++p
--                                 qq=(take qk (repeat 0))++q
--                                 lp=length p
--                                 lq=length q
--                                 pk=if (lp>lq) then lp-lq else 0
--                                 qk=if (lq>lp) then lq-lp else 0
  
--   (*) (Poly p) (Poly q) = Poly $ map (+) (map (\ (n) -> (let (hp : tp) = p in (map (* hp) q)++(take n (repeat 0)))) (reverse [0..(length p)])) 
    
--   fromInteger x = (Poly [fromInteger x])  
  
--   signum = undefined
--   abs = undefined