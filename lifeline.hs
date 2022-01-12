{-# LANGUAGE MultiWayIf #-}  --Подключаем расширения языка Haskell 
import Data.Char  --Подключаем расширения языка Haskell Data.Char  (reverse (dropWhile (<m) (reverse (dropWhile (< m) list))
import Data.List
import Data.Ord
import GHC.Float

-- slice :: Ord a => [a] -> [a]
-- slice l = let m = (maximum l)
--   in if m == maximum (delete (m) l)  
--       then (reverse (dropWhile (<m) (reverse (dropWhile (<m) l))))
--       else []



-- -- sumDivision x 2 
-- sumDivisionCalc :: Integer -> Integer -> Integer
-- sumDivisionCalc n x = if ((n^2) > x)
--   then 0
--   else if ((mod x n) == 0) 
--     then if (x `div` n) ^2 <= x 
--       then (n + ( sumDivisionCalc (n + 1) x))
--       else (n + (div x n) + ( sumDivisionCalc (n + 1) x))
--     else sumDivisionCalc (n + 1) x 

-- sumDivision :: Integer -> Integer
-- sumDivision x = 1 + (sumDivisionCalc 2 x)

-- friendNumbers :: Integer -> [[Integer]]
-- friendNumbers x =
--   if x == 0 
--     then []
--     else 
--       let y = sumDivision x
--       in 
--         if y < x && sumDivision y == x
--           then [x, y] : friendNumbers (x - 1) 
--           else friendNumbers (x - 1)

-- sumArr :: [Integer] -> [Integer] -> [Integer]
-- sumArr p q = zipWith (+) pp qq
--               where pp=p++(take pk (repeat 0))
--                     qq=q++(take qk (repeat 0))
--                     lp=length p
--                     lq=length q
--                     qk=if (lp>lq) then lp-lq else 0
--                     pk=if (lq>lp) then lq-lp else 0

-- mZipWith :: [Integer] -> [Integer] -> [Integer] -> [[Integer]]
-- mZipWith p q x = if (length p == 1 || length q == 1) 
--   then let (hp : tp) = p in let (hq : tq) = q in [(map (* hq) x) ++ (take (fromIntegral hp) (repeat 0))]
--   else let (hp : tp) = p in let (hq : tq) = q in [(map (* hq) x) ++ (take (fromIntegral hp) (repeat 0))] ++ (mZipWith tp tq x)
            

-- newtype Poly = Poly [Integer] deriving (Show, Eq)

-- degre :: Poly -> Int
-- degre (Poly p) = (length p)-1
 
-- value :: Poly -> Integer -> Integer
-- value (Poly p) x = foldl (\ acc v -> acc*x+v) 0 p

-- derivative :: Poly -> Poly
-- derivative p = Poly $ zipWith (*) [n, (n - 1) ..0] pl
--                 where n = (fromIntegral $ degre p)
--                       Poly pl = p

-- calcMultiplication :: Poly -> Poly -> Integer -> [Integer]
-- calcMultiplication (Poly p) (Poly q) n = map (* hp) q ++ replicate (fromIntegral n) 0
--                                                     where (hp : tp) = p

-- instance  Num Poly where
  
--   negate (Poly p) = (Poly $ map negate p)
  
--   (+) (Poly p) (Poly q) = (Poly $ zipWith (+) pp qq)
--                           where pp=p++(take pk (repeat 0))
--                                 qq=q++(take qk (repeat 0))
--                                 lp=length p
--                                 lq=length q
--                                 qk=if (lp>lq) then lp-lq else 0
--                                 pk=if (lq>lp) then lq-lp else 0
  
--   -- (*) (Poly p) (Poly q) = Poly $ foldl (sumArr) [0] (mZipWith (reverse [0..(fromIntegral $ ((-) (length p) 1))]) p) 
    
--   fromInteger x = (Poly [fromInteger x])  
  
--   signum = undefined
--   abs = undefined


distance :: (Integer,Integer) -> (Integer,Integer) -> Double
distance (x1, y1) (x2, y2) = sqrt . fromIntegral $ mx^2 + my^2
            where mx = x1 - x2
                  my = y1 - y2 

perimeter :: [(Integer,Integer)] -> Double
perimeter l = sum $ zipWith distance l (t++[h])
                where (h : t) = l
                      bl = [h]++l
                      nl = l++[h]