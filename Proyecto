-- Proyecto 1
-- Ejercicio 1
-- a)
esCero :: Int -> Bool
esCero x | x == 0 = True
         | otherwise = False
-- b)
esPositivo :: Int -> Bool
esPositivo x | x > 0 = True
             | otherwise = False
-- c)
esVocal :: Char -> Bool
esVocal x | x == 'a' = True
 | x == 'e' = True
 | x == 'i' = True
 | x == 'o' = True
 | x == 'u' = True
 | otherwise = False

-- Ejercicio 2
-- a)
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) | x == True = paratodo xs
                | otherwise = False
-- b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
-- c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs
-- d)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
-- e)
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (length xs)

-- Ejercicio 3
pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | x /= y = pertenece x ys

-- Ejercicio 4
-- a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) f | f (x) == True = paratodo' xs f
                   | otherwise = False
-- b) 
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = False
existe' (x:xs) f | f (x) == True = True
                 | otherwise = existe' xs f
-- c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x:xs) f = f (x) + sumatoria' xs f
-- d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) f = f (x) * productoria' xs f

-- Ejercicio 5
paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs (==True)

-- Ejercicio 6
-- a)
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs (even)
-- b)
-- FUNCION AUXILIAR
esMultiplo :: Int -> Int -> Bool
esMultiplo x y = mod y x == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo y xs = existe' xs (esMultiplo y)
-- c)
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n] (^2)
-- d)
factorial' :: Int -> Int
factorial' x = productoria' [0..x-1] (+1)
-- e)
-- FUNCION AUXILIAR
sonPares :: Int -> Int
sonPares x | (mod x 2 == 0) = x 
           | otherwise = 1

multiplicaPares :: [Int] -> Int
multiplicaPares (x:xs) = productoria' xs (sonPares)

-- Ejercicio 7
{-
 La funcion map toma una funcion y una lista y aplica esa funcion a cada elemento de la lista, dando como resultado una nueva lista
 
 La funcion filter toma una funcion y una lista y devuelve una nueva lista compuesta de los elementos de la lista original que 
 cumplen con la funcion

map succ [1, -4, 6, 2, -8] donde succ n = n+1, devuelve el sucesor de cada elemento de la lista, por tanto el resultado 
seria = [2, -3, 7, 3, -7]

filter esPositivo [1, -4, 6, 2, -8], devuelve todos los elementos de la lista que sean positivos, por tanto el resultado
seria = [1, 6, 2] 
-}

-- Ejercicio 8
-- a)
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x * 2 : duplica xs

-- b)
duplica' :: [Int] -> [Int]
duplica' xs = (map (*2) xs)

-- Ejercicio 9
-- a)
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | (mod x 2 == 0) = x : soloPares xs
                 | otherwise = soloPares xs
-- b)
soloPares' :: [Int] -> [Int]
soloPares' xs = filter even xs 
-- c)
multiplicaPares' :: [Int] -> Int
multiplicaPares' (x:xs) = productoria (soloPares' xs)

-- Ejercicio 10
-- a)
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA y (x:xs) | (y == x) = x : primIgualesA y xs
                      | otherwise = []

-- b)
primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' y xs = takeWhile (==y) xs

-- Ejercicio 11
-- a)
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs) | (x == y) = x:y: primIguales xs
                     | otherwise = [x]

-- b)
primIguales' :: Eq a => [a] -> [a]
primIguales' xs = primIgualesA' (head xs) xs

-- Ejercicio 12
-- Primera parte
cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen _ z [] _ = z
cuantGen op z (x:xs) t = (op) (t x) (cuantGen op z xs t)

--Segunda parte
paratodo''' xs f = cuantGen (&&) True xs f
existe'' xs f = cuantGen (||) False xs f
sumatoria'' xs f = cuantGen (+) 0 xs f 
productoria'' xs f = cuantGen (*) 1 xs f

{- Ejercicio 13
a) Sí está bien tipado. Su tipo es una tupla, donde el primer valor puede ser de cualquier tipo 
y el segundo valor también puede ser de cualquier tipo, es decir, una tupla polimórfica paramétrica.
El patron si cumple con todos los casos de la definicion.
x se relaciona con a, e y se relaciona con b.

b) Esta mal tipada, pues la definicion espera una lista de tuplas sin embargo en el patron solo 
se encuentra una tupla.

c) Sí está bien tipado. Su tipo es una lista de tuplas, donde el primer valor puede ser de cualquier tipo 
y el segundo valor también puede ser de cualquier tipo. 
El patron no cumple con todos los casos de la definicion, pues falta el caso vacio.
x se relaciona con (a,b), y xs sería el resto de tuplas de la lista.

d) Sí está bien tipado. Su tipo es una lista de tuplas, donde el primer valor puede ser de cualquier tipo 
y el segundo valor también puede ser de cualquier tipo.
El patron no cumple con todos los casos de la definicion, pues falta el caso vacio.
x se relaciona con a, y se relaciona con b, a se relaciona con a y b se relaciona con b (la forma en la que
esta escrito es extraña, generalmente la variable del patron no seria la misma letra que la definicion.
Esto no afecta al funcionamiento sin embargo) y xs sería el resto de tuplas de la lista

e) Sí está bien tipado. Su tipo es una lista de tuplas, donde el primer valor debe ser un entero y el 
segundo valor puede ser de cualquier tipo. falta lista de mas elementos caso vacio donde el primer elemento de la tupla no sea 0
El patron no cumple con todos los casos de la definicion, pues falta el caso vacio y los caso en los que el primer elemento no es 0.
0 se relaciona con Int y a se relaciona con a

f) No esta bien tipado, pues el 1 de el patron impide que a de la definicion pueda ser otro tipo.

g) Sí está bien tipado. Su tipo es una funcion que va de entero a entero y un entero.
El patron si cumple con todos los casos de la definicion.
a se relaciona con la funcion (Int -> Int) y b se relaciona con el Int.

h) Sí está bien tipado. Su tipo es una funcion que va de entero a entero y un entero.
El patron no cumple con todos los casos de la definicion, pues faltan los casos en los que el entero no sea 3.
a se relaciona con la funcion (Int -> Int) y 3 se relaciona con el Int.

i) No esta bien tipado, pues en el patron no hay una funcion como se pide en la definicion, en cambio hay entero que no se piden.
-}

-- Ejercicio 14
{-
a) f :: (a, b) -> b
f (x,y) = y
o tambien
f (z,w) = snd (z,w)


b) f :: (a, b) -> c
No es posible hacer una definición.

c) f :: (a -> b) -> a -> b
f g x = g x
o tambien
($) es un ejemplo


d) f :: (a -> b) -> [a] -> [b]
f funcion (x:xs)= funcion x : f funcion xs 
o tambien
map es un ejemplo


e) f :: (a -> b) -> (b -> c) -> a -> c
f funcion1 funcion2 z = funcion2 (funcion1 z)

