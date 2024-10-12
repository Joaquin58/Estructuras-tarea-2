{-
  Módulo a importar que contiene las funciones a testear.
  Author: Sebastián Alamina Ramírez
-}
{-# LANGUAGE GADTs #-}
import Practica02 -- Módulo con las funciones a testear.
import Data.List -- Funciones como 'sort'.

-- Tipo de dato para pruebas con una sola respuesta.
data PruebaSimple t where PruebaS :: (Show t, Eq t) => t -> t -> PruebaSimple t
instance Show (PruebaSimple a) where 
    show (PruebaS x y) = if x == y
                         then "Pasó."
                         else "No pasó...\n  Respuesta esperada: " ++ show y ++ "\n  Respuesta dada: " ++ show x

-- Tipo de dato para pruebas con múltiples respuestas posibles.
data PruebaMultiple t where PruebaM :: (Show t, Eq t) => t -> [t] -> PruebaMultiple t
instance Show (PruebaMultiple a) where
    show (PruebaM x l)
        | l!!0 == x = "Pasó. Y se eliminan variables repetidas, ¡bien!"
        | l!!1 == x = "Pasó. Pero no se eliminan variables repetidas..."
        | otherwise = "No pasó...\n  Posibles respuestas: " ++ show l ++ "\n  Respuesta dada: " ++ show x

-- Proposiciones lógicas.
x = Var "x"
p = Var "P"
q = Var "Q"
r = Var "R"
s = Var "S"
p0 = Impl p q
p1 = Impl (Impl p q) (Conj (Neg r) (Disy p PTrue))
p2 = Neg (Conj (Impl p q) (Syss q (Disy r s)))
p3 = Impl (Conj (Impl p q) p) q
p4 = Syss (Disy p q) (Neg (Disy p q))
-- Las siguientes resultan de agregar Neg's de más a p0,p1,p2,p3,p4 respectivamente.
p5 = Impl (Neg (Neg p)) q
p6 = Impl (Neg (Neg (Impl p q))) (Conj (Neg (Neg (Neg r))) (Disy p (Neg (Neg PTrue))))
p7 = Neg (Conj (Impl p q) (Neg (Neg (Syss (Neg (Neg (Neg (Neg q)))) (Disy r s)))))
p8 = Impl (Neg (Neg (Conj (Impl p q) p))) q
p9 = Syss (Disy (Neg (Neg p)) q) (Neg (Neg (Neg (Disy p (Neg (Neg q))))))
p10 = Neg (Conj (Disy PTrue PFalse) (PTrue))

-- Función principal.
main = do

    print "----- vars -----"
    print (PruebaS (vars PTrue) [])
    print (PruebaS (vars PFalse) [])
    print (PruebaS (vars x) ["x"])
    print (PruebaS (vars p0) ["P", "Q"])
    print (PruebaM (sort (vars p1)) [["P", "Q", "R"], ["P", "P", "Q", "R"]])
    print (PruebaM (sort (vars p2)) [["P", "Q", "R", "S"], ["P", "Q", "Q", "R", "S"]])
    print (PruebaM (sort (vars p3)) [["P", "Q"], ["P", "P", "Q", "Q"]])
    print (PruebaM (sort (vars p4)) [["P", "Q"], ["P", "P", "Q", "Q"]])

    print "----- deMorgan -----"
    print (PruebaS (deMorgan PTrue) PTrue)
    print (PruebaS (deMorgan PFalse) PFalse)
    print (PruebaS (deMorgan x) x)
    print (PruebaS (deMorgan p0) p0)
    print (PruebaS (deMorgan p1) p1)
    print (PruebaS (deMorgan p2) (Disy (Neg (Impl p q)) (Neg (Syss q (Disy r s)))))
    print (PruebaS (deMorgan p3) p3)
    print (PruebaS (deMorgan p4) (Syss (Disy p q) (Conj (Neg p) (Neg q))))
    print (PruebaS (deMorgan p10) (Disy (Conj PFalse PTrue) PFalse))

    print "----- equiv_op -----"
    print (PruebaS (equiv_op PTrue) PTrue)
    print (PruebaS (equiv_op PFalse) PFalse)
    print (PruebaS (equiv_op x) x)
    print (PruebaS (equiv_op p0) (Disy (Neg (Var "P")) (Var "Q")))
    print (PruebaS (equiv_op p1) (Disy (Neg (Disy (Neg (Var "P")) (Var "Q"))) (Conj (Neg (Var "R")) (Disy (Var "P") PTrue))))
    print (PruebaS (equiv_op p2) (Neg (Conj (Disy (Neg (Var "P")) (Var "Q")) (Conj (Disy (Neg (Var "Q")) (Disy (Var "R") (Var "S"))) (Disy (Neg (Disy (Var "R") (Var "S"))) (Var "Q"))))))
    print (PruebaS (equiv_op p3) (Disy (Neg (Conj (Disy (Neg (Var "P")) (Var "Q")) (Var "P"))) (Var "Q")))
    print (PruebaS (equiv_op p4) (Conj (Disy (Neg (Disy (Var "P") (Var "Q"))) (Neg (Disy (Var "P") (Var "Q")))) (Disy (Neg (Neg (Disy (Var "P") (Var "Q")))) (Disy (Var "P") (Var "Q")))))

    print "----- dobleNeg -----"
    print (PruebaS (dobleNeg PTrue) PTrue)
    print (PruebaS (dobleNeg PFalse) PFalse)
    print (PruebaS (dobleNeg x) x)
    print (PruebaS (dobleNeg p0) p0)
    print (PruebaS (dobleNeg p1) p1)
    print (PruebaS (dobleNeg p2) p2)
    print (PruebaS (dobleNeg p3) p3)
    print (PruebaS (dobleNeg p4) p4)
    print (PruebaS (dobleNeg p5) p0)
    print (PruebaS (dobleNeg p6) p1)
    print (PruebaS (dobleNeg p7) p2)
    print (PruebaS (dobleNeg p8) p3)
    print (PruebaS (dobleNeg p9) p4)

    print "----- num_conectivos -----"
    print (PruebaS (num_conectivos PTrue) 0)
    print (PruebaS (num_conectivos PFalse) 0)
    print (PruebaS (num_conectivos x) 0)
    print (PruebaS (num_conectivos p0) 1)
    print (PruebaS (num_conectivos p1) 5)
    print (PruebaS (num_conectivos p2) 5)
    print (PruebaS (num_conectivos p3) 3)
    print (PruebaS (num_conectivos p4) 4)
    print (PruebaS (num_conectivos p5) 3)
    print (PruebaS (num_conectivos p6) 11)
    print (PruebaS (num_conectivos p7) 11)
    print (PruebaS (num_conectivos p8) 5)
    print (PruebaS (num_conectivos p9) 10)

    print "----- num_variables -----"
    print (PruebaS (num_variables PTrue) 0)
    print (PruebaS (num_variables PFalse) 0)
    print (PruebaS (num_variables x) 1)
    print (PruebaS (num_variables p0) 2)
    print (PruebaM (num_variables p1) [3, 4])
    print (PruebaM (num_variables p2) [4, 5])
    print (PruebaM (num_variables p3) [2, 4])
    print (PruebaM (num_variables p4) [2, 4])

    print "----- profundidad -----"
    print (PruebaS (profundidad PTrue) 0)
    print (PruebaS (profundidad PFalse) 0)
    print (PruebaS (profundidad x) 0)
    print (PruebaS (profundidad (Neg (Neg PTrue))) 2)
    print (PruebaS (profundidad p0) 1)
    print (PruebaS (profundidad p1) 3)
    print (PruebaS (profundidad p2) 4)
    print (PruebaS (profundidad p3) 3)
    print (PruebaS (profundidad p4) 3)
    print (PruebaS (profundidad p5) 3)
    print (PruebaS (profundidad p6) 5)
    print (PruebaS (profundidad p7) 9)
    print (PruebaS (profundidad p8) 5)
    print (PruebaS (profundidad p9) 7)

    print "----- interpretacion -----"
    print (PruebaS (interpretacion PTrue []) True)
    print (PruebaS (interpretacion PFalse []) False)
    print (PruebaS (interpretacion x [("x", True)]) True)
    print (PruebaS (not $ interpretacion x [("x", False)]) True)
    print (PruebaS (not $ interpretacion p0 [("P", True), ("Q", False)]) True)
    print (PruebaS (interpretacion p1 [("P", True), ("Q", True) , ("R", False)]) True)
    print (PruebaS (interpretacion p2 [("P", True), ("Q", False), ("R", False), ("S", True)]) True)
    print (PruebaS (interpretacion p3 [("P", False), ("Q", False)]) True)
    print (PruebaS (not $ interpretacion p4 [("P", True), ("Q", True)]) True)
    print (PruebaS (interpretacion p0 [("P", True), ("Q", False)]) (interpretacion p5 [("P", True), ("Q", False)]))
    print (PruebaS (interpretacion p1 [("P", True), ("Q", True) , ("R", False)]) (interpretacion p6 [("P", True), ("Q", True) , ("R", False)]))
    print (PruebaS (interpretacion p2 [("P", True), ("Q", False), ("R", False), ("S", True)]) (interpretacion p7 [("P", True), ("Q", False), ("R", False), ("S", True)]))
    print (PruebaS (interpretacion p3 [("P", False), ("Q", False)]) (interpretacion p8 [("P", False), ("Q", False)]))
    print (PruebaS (interpretacion p4 [("P", True), ("Q", True)]) (interpretacion p9 [("P", True), ("Q", True)]))
