-- XOR
(-||) :: Bool -> Bool -> Bool
(-||) p q = (p||q) && (not (p && q))

-- Implicacion
(-->) :: Bool -> Bool -> Bool
(-->) p q = not (p && (not q))

-- Pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Muy mal"
head' (x:_) = x
 
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

ladder :: (Ord a) => [a] -> [a]
ladder [] = []
ladder [x] = [x]
ladder (x:y:xs) = x : (if (x <= y) then ladder (y:xs) else [])


sumEven :: (Integral a) => [a] -> a
sumEven [] = 0
sumEven (x:xs) = (if x `mod` 2 == 0 then x else 0) + sumEven xs

-- Typeclasses

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)



f [] l ls = ls ++ [reverse l]
f (n:ns) [] ls = f ns [n] ls
--f (n:ns) (m:ms) ls | n >= m = f ns (n:m:ms) ls
--                   | otherwise = f ns [n] (ls ++ [reverse (m:ms)])
{-

ascli = ascli' []

ascli' lo [] = lo
ascli' lo (x:xs)
	| xs == [] = ascli' (lo ++ [x]) []
	| otherwise = 
-}

{-
ascli' lo li
 | li == [] = lo
 | otherwise = ascli' (lo ++ [crec]) (drop (length crec) li)
 where crec = ([li!!0] ++ [li!!i | i <-[1..(length li)-1], li!!(i-1) <= li!!i])
 -}
 
{-
ascli' lo li
 | li == [] = lo
 | let crec = ([li!!0] ++ [li!!i | i <-[1..(length li)-1], li!!(i-1) <= li!!i]),
   otherwise = ascli' (lo ++ [crec]) (drop (length crec) li)
-}

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n li
 | n <= 0      = []
take' n []     = []
take' n (x:xs) = x : take' (n-1) xs


zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (xa:xas) (xb:xbs) = (xa, xb) : zip' xas xbs


-- Yo
qs' :: (Ord a) => [a] -> [a]
qs' []     = []
qs' (x:xs) = (qs' [y | y <- xs, y <= x]) ++ [x] ++ (qs' [y | y <- xs, y > x])

-- Como el tutorial
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

{-
	Currificación usando orden superior
-}

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

square :: (Num a) => a -> a
square x = x*x

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- Por currificación, toma una función que toma un elemento a y devuelve b,
-- y como segundo parámetro una lista "a", a lo que devuelve una lista de "b".
map' :: (a -> b) -> [a] -> [b] 
map' _ [] = []
map' f (x:xs) = (f x) : (map' f xs)

-- Espar2 ejemplo
esPar2 :: Int -> Bool
esPar2 n = mod n 2 == 0

{-
Vamos a buscar el número más grande por debajo de 100.000 que sea divisible por 3829.
Para lograrlo, simplemente filtramos un conjunto de posibilidades en el cual sabemos que está la solución.
-}
ejDiv3829' :: (Integral a) => a
ejDiv3829' = 
  let 
    p a = a `mod` 3829 == 0 
  in 
   head (filter p (reverse [0..100000]))


-- Secuencia de Collatz
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | n `mod` 2 == 0 = n : (collatz (div n 2))
  | otherwise = n : (collatz (3*n + 1))


todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d) = (a < b) && (b < d)



-- # # # # Práctica 3
-- Digito unidades
digitoUnidades :: (Integral a) => a -> a
digitoUnidades a = mod a 10

-- Alguno es cero
algunoEs0 :: (Integral a) => a -> a -> Bool
algunoEs0 x y
  | (x == 0) || (y == 0) = True
  | otherwise            = False

-- Ambos son cero
ambosSon0 :: (Integral a) => a -> a -> Bool
ambosSon0 0 0 = True
ambosSon0 _ _ = False

-- Suma N impares
sumaPrimerosNImpares :: Integer -> Integer
sumaPrimerosNImpares n
  | n == 1                = 1
  | n > 1 && mod n 2 == 0 = sumaPrimerosNImpares (n-1)
  | otherwise             = n + sumaPrimerosNImpares (n-1)

-- Recursión doble
sumInterna :: Integer -> Integer -> Integer
sumInterna _ 0 = 0
sumInterna n j = n^j + sumInterna n (j-1)

sumDoble :: Integer -> Integer -> Integer
sumDoble 0 _ = 0
sumDoble n m = sumInterna n m + sumDoble (n-1) m 


-- Suma de los digitos de un numero natural
sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

--
digIguales :: Integer -> Bool
digIguales n
  | n < 10                        = True
  | mod (div n 10) 10 /= mod n 10 = False
  | otherwise                     = digIguales (div n 10)



cantidadElementos :: [a] -> [Char]
cantidadElementos (x:y:ys) = "Hay al menos DOS elementos en al lista"
cantidadElementos (x:ys) = "Hay un solo elemento en la lista"
cantidadElementos _ = "La lista no tiene elementos"

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b
  | mod a b == 0 = True
  | otherwise    = False


-- import qualified Data.List as L

--transpose' :: [[a]] -> [[a]]
--transpose' :: n l@(m:ms) = take (maximum $ map length l) $ repeat []

-- transpose'aux n [] =
-- transpose'aux nm xs =

-- trs xs [] = xs
trs xs m
  | length rem > 0 = [foldl (++) [] (fstel ms)] ++ (trs [] $ rem)
  | otherwise      = xs
  where
    fstel = map (\x -> [head x])
    filt s = filter (\y -> length y > 0) s
    ms = filt m
    rem = map tail ms
    

sumatoria :: (Integer -> Integer) -> Integer -> Integer
sumatoria a 0 = 0
sumatoria a n = a n + sumatoria a (n-1)


-- Guia 3
g :: Integer -> Integer
g 8   = 16
g 16  = 4
g 131 = 1


-- 2.1 absoluto
abs' :: Integer -> Integer
abs' x 
  | x >= 0 = x
  | otherwise = (-1)*x



    

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n-2)


-- Guia de recursión

-- ej. 1:
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2) 

-- ej. 2:
parteEntera :: Float -> Integer
parteEntera x
    | x <= (-1) = (-1) + parteEntera (x+1)
    | x < 1     = 0
    | otherwise = 1 + parteEntera (x-1)
-- resuelto segun la guía
parteEnteraProfe :: Float -> Integer
parteEnteraProfe x
    | 0 < x && x < 1 = 0
    | x >= 1       = 1 + parteEnteraProfe (x-1)
    | otherwise    = (-1) + parteEnteraProfe (x+1)


-- ej. 3:
{-
problema esDivisible (a,b:Z): Bool {
  requiere: {b /= 0}
   asegura: {a mod b == 0}
}
-}
esDivisible :: Integer -> Integer -> Bool
esDivisible a b = esDivisible' (abs a) (abs b)

esDivisible' :: Integer -> Integer -> Bool
esDivisible' a b 
  | a == 0    = True
  | a < 0     = False
  | otherwise = esDivisible' (a-b) b

-- ej. 4:
{-
problema sumaImpares (n:N) : N {
 requiere: {True}
  asegura: {res=Σ[k=0,n-1](2k+1)}
}
-}
sumaImpares2 :: Integer -> Integer
sumaImpares2 1 = 1
sumaImpares2 n
  | mod n 2 == 0 = sumaImpares2 (n-1)
  | otherwise    = n + sumaImpares2 (n-2)
  
-- Como lo resolví antes
sumaImpares :: Integer -> Integer
sumaImpares 1 = 1
sumaImpares n
    | mod n 2 /= 0 = n + sumaImpares (n-1)
    | otherwise    = 0 + sumaImpares (n-1)

-- ej 5:


-- Es capicua
esCapicua :: Integer -> Bool
esCapicua n = n == reverso' 0 n

reverso' :: Integer -> Integer -> Integer
reverso' n m
  | m < 10   = n*10 + m
  |otherwise = reverso' (n*10 + mod m 10) (div m 10)

-----

todosEq :: (Eq a) => [a] -> Bool
todosEq (x:y:ys)
  | x /= y = False
  | otherwise = todosEq (y:ys)
todosEq _ = True
  
-- ej 11:
fact :: Integer -> Integer
fact 0 = 1
fact n = n*fact (n-1)

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = 1/(fromIntegral (fact n)) + eAprox (n-1)

-- b)
e :: Float
e = eAprox 10

-- ej 12:
terminoGeneralRaiz2 :: Integer -> Float
terminoGeneralRaiz2 1 = 2
terminoGeneralRaiz2 n = 2 + 1/(terminoGeneralRaiz2 $ n-1)

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = terminoGeneralRaiz2 n - 1

-- ej 13:
f13 :: Integer -> Integer -> Integer
f13 1 m = f13' 1 m 
f13 n m = f13' n m + f13 (n-1) m

f13' :: Integer -> Integer -> Integer
f13' i 1 = i
f13' i j = i^j + f13' i (j-1)

-- ej 14:
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q 1 m = sumaPotencias' q 1 m
sumaPotencias q n m = sumaPotencias' q n m + sumaPotencias q (n-1) m

sumaPotencias' :: Integer -> Integer -> Integer -> Integer
sumaPotencias' q a 1 = funPotencia q a 1
sumaPotencias' q a b = funPotencia q a b + sumaPotencias' q a (b-1)

funPotencia :: Integer -> Integer -> Integer -> Integer
funPotencia q a b = q^(a+b)

-- ej 15:
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 1 m = sumaRacionales' 1 m
sumaRacionales n m = sumaRacionales' n m + sumaRacionales (n-1) m 

sumaRacionales' :: Integer -> Integer -> Float
sumaRacionales' p 1 = fromIntegral p
sumaRacionales' p q = (/) (fromIntegral p) (fromIntegral q)
                      + sumaRacionales' p (q-1)

-- ej 16:
