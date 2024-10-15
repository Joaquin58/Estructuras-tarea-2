module Practica02 where

import Data.List (nub)  -- Necesario para eliminar duplicados de la lista

-- Definición del tipo de dato LProp
data LProp = PTrue | PFalse | Var Nombre 
           | Neg LProp | Conj LProp LProp | Disy LProp LProp 
           | Impl LProp LProp | Syss LProp LProp
           deriving (Eq, Show)

type Nombre = String

-- Función 2: deMorgan
deMorgan :: LProp -> LProp
deMorgan (Neg (Conj p q)) = Disy (Neg p) (Neg q)  -- Aplica ¬(p ∧ q) -> ¬p ∨ ¬q
deMorgan (Neg (Disy p q)) = Conj (Neg p) (Neg q)  -- Aplica ¬(p ∨ q) -> ¬p ∧ ¬q
deMorgan f = f  -- Si no es aplicable, regresa la fórmula original

-- Función 4: dobleNeg
dobleNeg :: LProp -> LProp
dobleNeg (Neg (Neg p)) = p  -- Elimina la doble negación
dobleNeg f = f  -- Si no hay doble negación, regresa la fórmula original

-- Función variables (4)
varsAux :: LProp -> [Nombre]
varsAux PTrue = []
varsAux PFalse = []
varsAux (Var v) = [v]
varsAux (Neg p) = varsAux p
varsAux (Conj p q) = varsAux p ++ varsAux q
varsAux (Disy p q) = varsAux p ++ varsAux q
varsAux (Impl p q) = varsAux p ++ varsAux q
varsAux (Syss p q) = varsAux p ++ varsAux q

-- Función 6: num_variables corregida para contar solo variables únicas
num_variables :: LProp -> Int
num_variables f = length (nub (varsAux f))
