-- Proyecto 2
-- Ejercicio 1)
-- a)
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq,Show)

-- b)
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de La Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

-- c)
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord, Show, Bounded)

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do  = 'C'
cifradoAmericano Re  = 'D'
cifradoAmericano Mi  = 'E'
cifradoAmericano Fa  = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La  = 'A'
cifradoAmericano Si  = 'B'

-- Ejercicio 2)
-- Se agrego Eq, Ord y Show

-- Ejercicio 3)
-- a)
minimoElemento :: Ord a => [a] -> a
minimoElemento xs = minimum xs
-- o se puede usar tambien x `min` minimoElemento' xs

-- b)
minimoElemento' :: (Bounded a) => (Ord a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = x `min` minimoElemento' xs

--c)
-- minimoElemento [Do,Re,Mi,Fa,Sol,La,Si]
-- Do
-- Al estar ya ordenado del mas grave a agudo, la funcion minimoElemento nos dice cual es la mas grave.

-- Ejercicio 4)
-- a)
-- Ingreso es un sinonimo de tipo.
type Ingreso = Int
-- Cargo y Area son tipos enumerados
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq,Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq,Show)
-- Persona es un tipo algebraico
data Persona = Decane -- constructor sin argumento
            | Docente Cargo -- constructor con un argumento
            | NoDocente Area -- constructor con un argumento
            | Estudiante Carrera Ingreso -- constructor con dos argumentos
            deriving (Eq, Show)
-- b)
-- El tipo del contructor Docente es Cargo -> Persona
-- Docente :: Cargo -> Persona

-- c)
cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (x:xs) c | (x == Docente c) = 1 + cuantos_doc xs c
                     | otherwise = cuantos_doc xs c
-- Prueba : cuantos_doc [Docente Titular, Docente Asociado, Docente Asistente, Docente Auxiliar] Asistente 
-- 1

-- d)
cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' (xs) c = length (filter (== Docente c) xs)

-- Ejercicio 5)
-- a)
data Alteracion = Bemol | Sostenido | Natural
data NotaMusical = Nota NotaBasica Alteracion

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

-- b)
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota notaBasica Bemol) = (sonido notaBasica) -1
sonidoCromatico (Nota notaBasica Sostenido) = (sonido notaBasica) +1
sonidoCromatico (Nota notaBasica Natural) = (sonido notaBasica )

-- c)
instance Eq NotaMusical
    where
        Nota n1 a1 == Nota n2 a2 = sonidoCromatico (Nota n1 a1) == sonidoCromatico (Nota n2 a2)
-- (Nota Re Bemol) == (Nota Do Sostenido)

-- d)
instance Ord NotaMusical
    where
        Nota n1 a1 <= Nota n2 a2 = sonidoCromatico (Nota n1 a1) <= sonidoCromatico (Nota n2 a2)

-- Ejercicio 6)
-- a)
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (xs) = Just (head xs)

-- Ejercicio 7)

data Cola = VaciaC | Encolada Persona Cola deriving (Show)

-- a)
-- 1)
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p c) = Just (c)
-- atender (Encolada (Docente Titular) (Encolada (Decane) (VaciaC)))
-- Just (Encolada Decane VaciaC)

-- 2)
encolar :: Persona -> Cola -> Cola
encolar (ps) VaciaC = Encolada ps VaciaC
encolar (ps) (Encolada ps1 c) = Encolada ps1 (encolar ps c)
-- encolar (Docente Adjunto) (Encolada (Docente Titular) (Encolada (Decane) (VaciaC)))
-- Encolada (Docente Titular) (Encolada Decane (Encolada (Docente Adjunto) VaciaC))

-- 3)
busca :: Cola -> Cargo -> Maybe Persona
busca cola cargo = case cola of
                     VaciaC -> Nothing
                     (Encolada (Docente cargo) c) -> Just (Docente cargo)
                     (Encolada _ c) -> busca c cargo

{-

Forma de hacerlo sin case
busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC _ = Nothing
busca (Encolada p cola) cargo = if (Docente cargo == p)
                                    then Just p
                                    else busca cola cargo

-}
-- busca (Encolada Decane(Encolada (Docente Titular) ( VaciaC))) (Titular)
-- Just (Docente Titular)
-- busca' (Encolada Decane(Encolada (Docente Titular) ( VaciaC))) (Titular)
-- Just (Docente Titular)

-- b)
-- Cola se parece a tipo lista,  data List a = Nil | Cons a (List a), pues ambos son de forma recursiva, y de cierta forma Cola tambien es una lista pero acotada a solo Personas.

-- Ejercicio 8)
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show, Eq)

-- a)
type ListTel = ListaAsoc String Int

-- b)
-- 1)
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b lista) = 1 + (la_long lista)

-- 2)
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat (Vacia) (Vacia) = Vacia
la_concat (Vacia) a = a
la_concat a (Vacia) = a
la_concat (Nodo a1 b1 lista1) (Nodo a2 b2 lista2) = Nodo a1 b1 (la_concat (lista1) (Nodo a2 b2 lista2))

-- la_concat (Nodo 1 2 Vacia) (Nodo 4 5 Vacia)

-- 3)
la_agregar :: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia a1 b1 = (Nodo a1 b1 Vacia)
la_agregar (Nodo a b lista) a1 b1 = Nodo a b (la_agregar (lista) a1 b1)

-- 4)
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a1 b1 lista) = (a1,b1) : (la_pares lista)

-- 5)
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing
la_busca (Nodo a b lista) da | (a == da) = Just b
                             | otherwise = la_busca lista da

-- 6)
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar a Vacia = Vacia
la_borrar a (Nodo a1 b1 lista) | (a1 == a) = lista
                               | (a1 /= a) = Nodo a1 b1 (la_borrar a lista)

-- Ejercicio 9)
data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a )

type Prefijos = Arbol String
can , cana , canario , canas , cant , cantar , canto :: Prefijos
can = Rama cana "cant" cant
cana = Rama canario "a" canas
canario = Rama Hoja "rio" Hoja
canas = Rama Hoja "s" Hoja
cant = Rama cantar "t" canto
cantar = Rama Hoja "ar" Hoja
canto = Rama Hoja "o" Hoja

-- a) a_long :: Arbol a -> Int que dado un ́arbol devuelve la cantidad de datos almacenados.
a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama x _ y) = a_long x + 1 + a_long y


-- b) a_hojas :: Arbol a -> Int que dado un ́arbol devuelve la cantidad de hojas.
a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama x _ y) = a_hojas x + a_hojas y

--c) a_inc :: Num a => Arbol a -> Arbol a que dado un ́arbol que contiene números, los incrementa en uno.
a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama x n y) = Rama (a_inc x) (n+1) (a_inc y)

{- d) a_map :: (a -> b) -> Arbol a -> Arbol b que dada una función y un  ́arbol, devuele el arbol con la misma estructura, que resulta de aplicar la función a cada 
uno de los elementos del árbol. Revisá la definción de la función anterior y reprogramala usando a_map. -}
a_map :: (a->b) -> Arbol a -> Arbol b 
a_map _ Hoja = Hoja
a_map f (Rama x n y) = (Rama (a_map f x) (f n) (a_map f y))

a_inc' :: Num a => Arbol a -> Arbol a
a_inc' = a_map (+1)


--Tema A
-- Ejercicio 1
data EmpresaTelefono = Claro | Personal | Movistar | Tuenti deriving (Eq,Show)
type Frase = String

fraseEmpresa :: EmpresaTelefono -> Frase
fraseEmpresa Claro = "Claro, La red mas poderosa"
fraseEmpresa Movistar = "Movistar, Compartida la vida es mas..."
fraseEmpresa Tuenti = "Tuenti es la mas economica"
fraseEmpresa Personal = "Personal, es como vos"
-- fraseEmpresa Personal
-- "Personal, es como vos"

-- Ejercicio 2
type NombrePersona = String
data MisEmpresas = AgregaEmpresa EmpresaTelefono NombrePersona MisEmpresas | Ninguna deriving (Eq,Show)

tengoEmpresa :: MisEmpresas -> EmpresaTelefono -> NombrePersona -> Bool
tengoEmpresa Ninguna _ _ = False
tengoEmpresa (AgregaEmpresa a b restempres) empres name | ((a == empres) && (b == name)) = True
                                                        | otherwise = tengoEmpresa (restempres) empres name

-- tengoEmpresa (AgregaEmpresa Personal "ArneMuller" Ninguna) Personal "ArneMuller"
-- True

-- Ejercicio 3)
-- que devuelve la lista de asociaciones a la cual le agrego la asociación NroTel con la EmpresaTelefono.
type NroTel = Int
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show, Eq)
agregaLa :: ListaAsoc EmpresaTelefono NroTel -> EmpresaTelefono -> NroTel -> ListaAsoc EmpresaTelefono NroTel
agregaLa (Vacia) empres nrotel = Nodo empres nrotel Vacia
agregaLa (Nodo a b list) empres nrotel = Nodo a b (agregaLa (list) empres nrotel)

-- agregaLa (Nodo Personal 1234 Vacia) Movistar 12345
-- Nodo Personal 1234 (Nodo Movistar 12345 Vacia)

-- Ejercicio 4)
-- que dado un árbol y un valor de tipo a devuelve True si el valor está en el árbol y False en caso contrario.
data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a )
aBusca :: Eq a => Arbol a -> a -> Bool
aBusca Hoja _ = False
aBusca (Rama Hoja x Hoja) a | x==a = True
                            | otherwise = False
aBusca (Rama d1 x d2) a | (x == a) = True
                        | otherwise = (aBusca d1 a) == True || (aBusca d2 a) == True
                        | otherwise = False

-- Ejercicio 1)
data Forma = Piedra | Papel | Tijera
le_gana :: Forma -> Forma -> Bool
le_gana Piedra Piedra = False
le_gana Piedra Papel = False
le_gana Piedra Tijera = True
le_gana Papel Papel = False
le_gana Papel Tijera = False
le_gana Papel Piedra = True
le_gana Tijera Papel = True
le_gana Tijera Piedra = False
le_gana Tijera Tijera = False
-- b)
type Nombre = String
data Jugador = Mano Nombre Forma

quienGana :: Jugador -> Jugador -> Maybe Nombre
quienGana (Mano n1 f1) (Mano n2 f2) | le_gana f1 f2 == True = Just n1
                                    | le_gana f2 f1 == True = Just n2
                                    | otherwise = Nothing
ganador :: Jugador -> Jugador -> Maybe Nombre
ganador j1 j2 = quienGana j1 j2

ganador' :: Jugador -> Jugador -> Maybe Nombre
ganador' (Mano j1 f1) (Mano j2 f2) = case le_gana f1 f2 of
                                      True -> Just j1
                                      False -> case le_gana f2 f1 of
                                                               True -> Just j2
                                                               False -> Nothing

igual :: Forma -> Forma -> Bool
igual Piedra Piedra = True
igual Papel Papel = True
igual Tijera Tijera = True
igual _ _ = False
quien_jugo :: Forma -> [Jugador] -> [Nombre]
quien_jugo f [] = []
quien_jugo f ((Mano j1 f1):js) = case igual f f1 of
                                True -> (j1) : (quien_jugo f js)
                                False -> (quien_jugo f js)

--data NotaMusical = Do|  Re | Mi | Fa | Sol | La | Si
--data Figura = Negra | Corchea deriving (Eq)
--data Melodia = Entonar NotaMusical Figura Melodia | Vacia
--contar_tiempos :: Melodia -> Int
--contar_tiempos Vacia = 0
--contar_tiempos (Entonar note fig mellist) | fig == Negra = 2 + contar_tiempos mellist
 --                                         | fig == Corchea = 1+ contar_tiempos mellist

arbol_sum :: Arbol Int -> Arbol Int -> Arbol Int
arbol_sum Hoja Hoja = Hoja
arbol_sum (Rama Hoja b1 Hoja)  (Rama a2 b2 c)= Rama (a2) (b1+b2) (c)
arbol_sum (Rama a2 b2 c)  (Rama Hoja b1 Hoja)= Rama (a2) (b1+b2) (c)
arbol_sum (Rama z a r) (Rama z2 b r2) = Rama v z3 r3
                                        where
                                         v = arbol_sum z z2
                                         z3 = a + b
                                         r3 = arbol_sum r r

aCuantos :: Arbol Int -> Int -> Int
aCuantos Hoja x = 0
aCuantos (Rama a b c) x | b>x=1 + (aCuantos a x) + (aCuantos c x)
                        | otherwise = (aCuantos a x) + (aCuantos c x)
