module TP1(
  Fabrica, Material (..),
  crearFabricaSimple,
  neg, esPar, sumarTuplas,
  foldMaterial, crearFabricaDeMaterial,
  secuenciar, paralelizar,
  pureza, filtrarPorPureza,
  emparejador, crearFabricaCompleja
  ) where

import Test.HUnit
import Data.List

-- Definiciones

-- Fábricas son funciones [a] -> [b]
type Fabrica a b = [a] -> [b]

-- Materiales son un tipo algebráico
data Material a =
  MateriaPrima a |
  Mezclar (Material a) Float (Material a)
    deriving (Eq, Show)

-- Ej 1 a

crearFabricaSimple :: (a -> b) -> Fabrica a b
crearFabricaSimple f = map f

-- Ej 1 b

neg :: Fabrica Bool Bool
neg = crearFabricaSimple not

esPar :: Fabrica Int Bool
esPar = crearFabricaSimple even

sumarTuplas :: Num a => Fabrica (a, a) a
sumarTuplas = crearFabricaSimple (\t -> fst t + snd t)

-- Ej 2 

foldMaterial :: (a -> b) -> (b -> Float -> b -> b) -> Material a -> b
foldMaterial casoPrima casoMezcla mat =
    case mat of
        MateriaPrima a -> casoPrima a
        Mezclar p n q -> casoMezcla (resRec p) n (resRec q)
    where resRec = foldMaterial casoPrima casoMezcla


-- Ej 3

crearFabricaDeMaterial :: (a -> b) -> Fabrica (Material a) (Material b)
crearFabricaDeMaterial f = map (foldMaterial (MateriaPrima . f) (\p n q -> Mezclar  p n q))

-- Ej 4

secuenciar :: Fabrica a b -> Fabrica b c -> [a] -> [c]
secuenciar f1 f2 = f2 . f1

paralelizar :: Fabrica a c -> Fabrica b c -> [(a,b)] -> [c]
paralelizar factory1 factory2 xs =  entrelazar (zip (factory1 (map fst xs)) (factory2 (map snd xs)))

entrelazar :: [(c,c)] -> [c]
entrelazar = concatMap (\t -> [fst t, snd t])

-- Ej 5

pureza :: (Eq a) => a -> Material a -> Float
pureza base = foldMaterial (\prima -> if prima == base then 100 else 0)
                (\porcentaje1 proporcion porcentaje2 -> porcentaje1 * proporcion / 100 + porcentaje2 * ((100 - proporcion) / 100))

filtrarPorPureza :: (Eq a) => [Material a] -> [(a,Float)] -> [Material a]
filtrarPorPureza materiales restricciones = filter (materialCumpleRestricciones restricciones) materiales

materialCumpleRestricciones ::  (Eq a) =>  [(a,Float)] -> Material a -> Bool
materialCumpleRestricciones restricciones material = foldr (\r rec -> pureza (fst r) material >= snd r && rec ) True restricciones

-- Ej 6

emparejador :: [a] -> [(a,a)]
emparejador lista = zip (fst (paresEImpares lista)) (snd (paresEImpares lista))

paresEImpares :: [a] -> ([a], [a])
paresEImpares = foldr (\x rec -> (x:snd rec, fst rec)) ([], [])

crearFabricaCompleja :: (a -> a -> b) -> Fabrica a b
crearFabricaCompleja f = map (\t -> f (snd t) (fst t)) . emparejador

-- Tests

tests :: IO Counts
tests = do runTestTT allTests


allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6
  ]

-- Ejemplos sólo para mostrar cómo se escriben los tests. Reemplazar por los tests propios.

testsEj1 = test [
  [] ~=? neg [],
  [False] ~=? neg [True],
  [True] ~=? neg [False],
  [True, False, True] ~=? esPar [2,3,0],
  [] ~=? esPar [],
  [] ~=? sumarTuplas [],
  [4,6] ~=? sumarTuplas [(6,-2),(2,4)],
  [False, True, False, True, False] ~=? take 5 ((crearFabricaSimple even) [1..])
  ]

verdad = MateriaPrima True
mentira = MateriaPrima False

testsEj2 = test [
  1 ~=? foldMaterial id (\p n q -> p + q) (Mezclar (MateriaPrima 0) 50.0 (MateriaPrima 1)),
  False ~=? foldMaterial not (\p n q -> p && q) (Mezclar verdad 50.0 verdad),
  True ~=? foldMaterial not (\p n q -> p && q) mentira
  ]


testsEj3 = test [
  [Mezclar verdad 50.0 mentira, mentira] ~=? crearFabricaDeMaterial not [Mezclar mentira 50.0 verdad, verdad],
  [Mezclar mentira 50.0 verdad, verdad] ~=? crearFabricaDeMaterial even [Mezclar (MateriaPrima 1) 50.0 (MateriaPrima 2), MateriaPrima 0]
  ]

testsEj4 = test [
  [] ~=? secuenciar esPar neg [],
  [True, False, True, True] ~=? secuenciar esPar neg [1, 2, 3, 5],
  [] ~=? paralelizar neg esPar [],
  [False, False, True, False, True, False] ~=? paralelizar neg esPar [(True, 1), (False, 3), (False, 1)],
  [1,1,2,2,3] ~=? take 5 (entrelazar [(i,i) | i <- [1..]])
  ]



testsEj5 = test [
  100.0 ~=? pureza True (verdad),
  0.0 ~=? pureza True (mentira),
  25.0 ~=? pureza True (Mezclar (Mezclar verdad 50.0 mentira) 50.0 mentira),
  [Mezclar verdad 80.0 mentira] ~=? filtrarPorPureza [Mezclar verdad 44.5 mentira, Mezclar verdad 80.0 mentira, Mezclar mentira 99.0 verdad] [(True, 50.0), (False , 1.0)]
  ]

testsEj6 = test [
  [(1, 2), (3, 4)] ~=? emparejador [1, 2, 3, 4],
  [5, 9, 11] ~=? crearFabricaCompleja (+) [2, 3, 12, -3, 11, 0]
  ]

