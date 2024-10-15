-- UNIVERSIDAD NACIONAL AUTÓNOMA DE MEXICO
-- PRÁCTICA 02: Lógica Proposicional
-- Ramírez Mendoza Joaquín Rodrigo
-- Treviño Puebla Héctor Jerome
-- Villalobos Juárez Gontran Eliut

module Practica02 where 

data Lprop = PTrue | PFalse | Var Nombre | Neg Lprop 
            | Conj Lprop Lprop | Disy Lprop Lprop | Impl Lprop Lprop 
            | Syss Lprop Lprop deriving Eq

type Nombre = String
type Asignacion = [(String, Bool)]

instance Show Lprop where
    show PTrue = "True"
    show PFalse = "False"
    show (Var p) =  p
    show (Neg p) = "¬" ++  show p  
    show (Conj p q) = "(" ++ show p ++ "∧"   ++  show q ++ ")"
    show (Disy p q) = "(" ++ show p ++ "v"   ++  show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ "->"  ++  show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ "<->" ++  show q ++ ")"


--Función para eliminar los elementos repetidos de una lista
--Separa la lista en tope y el resto de la lista, si el elemento x
--se encuentra en el resto se queda con el resto, si no se queda con ambos
auxiliar_repetidos :: [String] -> [String]
auxiliar_repetidos [] = []
auxiliar_repetidos (x:xs) =
    if x `elem` xs
    then auxiliar_repetidos xs  
    else x : auxiliar_repetidos xs  

--Función que da las variables de una proposición logica
--Hace uso de la función auxiliar_repetidos para eliminar las variables repetidas
vars :: Lprop -> [String]
vars PTrue = []
vars PFalse = []
vars (Var n) = [n]
vars (Neg p) = auxiliar_repetidos (vars p) 
vars (Conj p q) = auxiliar_repetidos (vars p ++ vars q)
vars (Disy p q) = auxiliar_repetidos (vars p ++ vars q)
vars (Impl p q) = auxiliar_repetidos (vars p ++ vars q)
vars (Syss p q) = auxiliar_repetidos (vars p ++ vars q)

-- DeMorgan
deMorgan :: Lprop -> Lprop
deMorgan PTrue = PTrue
deMorgan PFalse = PFalse
deMorgan (Neg PTrue) = PFalse
deMorgan (Neg PFalse) = PTrue
deMorgan (Var n) = Var n
deMorgan (Neg (Conj p q)) = Disy (deMorgan (Neg p)) (deMorgan (Neg q))
deMorgan (Neg (Disy p q)) = Conj (deMorgan (Neg p)) (deMorgan (Neg q))
deMorgan (Neg p) = Neg (deMorgan p)
deMorgan (Conj p q) = Conj (deMorgan p) (deMorgan q)
deMorgan (Disy p q) = Disy (deMorgan p) (deMorgan q)
deMorgan (Impl p q) = Impl (deMorgan p) (deMorgan q)
deMorgan (Syss p q) = Syss (deMorgan p) (deMorgan q)

--Función que genera las  equivalencias lógicas de la Implicación y de la 
--doble implicación. Genera la equvalencia de la siguiente forma:
-- p-> q = -p v q
-- p <-> q = (-p v q)^(-q v p)
--Se llama recusivamente en todos los casos.
equiv_op :: Lprop -> Lprop
equiv_op PTrue = PTrue
equiv_op PFalse = PFalse
equiv_op (Var n) = (Var n)
equiv_op (Impl p q) = Disy (Neg (equiv_op p)) (equiv_op q)
equiv_op (Neg p) = Neg (equiv_op p)
equiv_op (Conj p q) = Conj (equiv_op p) (equiv_op q)
equiv_op (Disy p q) = Disy (equiv_op p) (equiv_op q)
equiv_op (Syss p q) = Conj (Disy (Neg (equiv_op p)) (equiv_op q)) (Disy (Neg (equiv_op q)) (equiv_op p))

dobleNeg :: Lprop -> Lprop
dobleNeg PTrue = PTrue
dobleNeg PFalse = PFalse
dobleNeg (Var n) = Var n
dobleNeg (Neg (Neg p)) = dobleNeg p
dobleNeg (Neg p) = Neg (dobleNeg p)
dobleNeg (Impl p q) = Impl  (dobleNeg p) (dobleNeg q)
dobleNeg (Disy p q) = Disy (dobleNeg p) (dobleNeg q)
dobleNeg (Conj p q) = Conj (dobleNeg p) (dobleNeg q)
dobleNeg (Syss p q) = Syss (dobleNeg p) (dobleNeg q)

--Función recursiva que cuenta el número de conectivos lógicos
--de una proposición. El caso base es con las variables y las contantes
-- Negación solo se agrega 1 a los conectivos de la Proposición dada
-- Operadores Binarios se agrega 1 a ambas proposiciones dadas
num_conectivos :: Lprop -> Integer
num_conectivos PTrue = 0
num_conectivos PFalse = 0
num_conectivos (Var n) = 0
num_conectivos (Neg p) = 1 + num_conectivos p
num_conectivos (Conj p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Disy p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Impl p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Syss p q) = 1 + num_conectivos p + num_conectivos q

num_variables :: Lprop -> Int
num_variables PFalse     = 0
num_variables (Var n)    = 1
num_variables (Neg p)    = num_variables p
num_variables (Impl p q) = (num_variables p) + (num_variables q)
num_variables (Disy p q) = (num_variables p) + (num_variables q)
num_variables (Conj p q) = (num_variables p) + (num_variables q)
num_variables (Syss p q) = (num_variables p) + (num_variables q)
    

--Se define la profundidad de una Expresión proposicional donde, el dato de entrada
--es una expresión proposicional del tipo LProp y devuelve un número entero
--el cual la máxima longitud entre dos ramas sumado 1
--el caso base son las variables atómicas y las constantes PTrue y PFalse, donde su 
--profundidad es cero.
profundidad :: Lprop -> Int
profundidad PTrue = 0
profundidad PFalse = 0
profundidad (Var n) = 0
profundidad (Neg p) = 1 + profundidad p
profundidad (Impl p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Disy p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Conj p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Syss p q) = 1 + max (profundidad p) (profundidad q)

{-
La interpretación de una expresión donde se reciben dos argumentos, un LProp y un arreglo que contiene
la asignación de cada una de las variables para la expresión.
Devuelve el valor de verdad final de la expresión para la interpretación dada.
-}
interpretacion :: Lprop -> Asignacion -> Bool
interpretacion PTrue  _ = True
interpretacion PFalse _ = False
interpretacion (Var p) asignacion    =  if interprete p asignacion == True then True else False
interpretacion (Neg p) asignacion    =  if interpretacion p asignacion == True then False else True
interpretacion (Conj p q) asignacion =  if interpretacion p asignacion == True && interpretacion q asignacion == True then True else False
interpretacion (Disy p q) asignacion =  if interpretacion p asignacion == True || interpretacion q asignacion == True then True else False
interpretacion (Impl p q) asignacion =  if interpretacion p asignacion == True && interpretacion q asignacion == False then False else True
interpretacion (Syss p q) asignacion =  if interpretacion p asignacion == interpretacion q asignacion then True else False
{-
Función auxiliar de la función interpretación el cual evalúa el valor de verdad de la variable
a la que se le asignó.
Regresa el valor de verdad para la variable en cuestión.
-}
interprete :: Nombre -> Asignacion -> Bool
interprete _ [] = error "error, no se asignaron valores"
interprete p ((x, var):xs) = if p == x then var else interprete p xs
