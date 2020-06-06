module Lib where
import Text.Show.Functions

laVerdad = True

{-De los ratones nos interesa modelar su nombre, su edad (en años), su peso, y las enfermedades que posee.
Por ejemplo:
Cerebro es un ratón con 9 años, 0.2 kg de peso y tiene brucelosis, sarampión y tuberculosis.
Bicenterrata es un ratón con 256 años, 0.2kg de peso, y completamente sano.
Huesudo es un ratón de 4 años con 10kg de peso, y alta obesidad y sinusitis.
Modelar a los ratones mencionados.-}

----Punto 1
data Raton = UnRaton { nombre :: String, 
                       edad :: Float, 
                       peso :: Float, 
                       enfermedades :: [String]} deriving (Show, Eq)

cerebro :: Raton
cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis", "sarampion", "tuberculosis"]                 

bicenterrata :: Raton
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []

huesudo :: Raton
huesudo = UnRaton "Huesudo" 4 10 ["Alta obsesidad", "sinusitis"]

----- Punto 2
--Existen distintos tipos de hierbas que afectan (modifican) de diferentes maneras al ratón. Definir dichas hierbas:
--a) hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.

type Hierba = Raton -> Raton 

hierbaBuena :: Hierba
hierbaBuena =  cambiarEdad sqrt 

cambiarEdad :: (Float -> Float) -> Hierba 
cambiarEdad funcion raton = raton {edad = (funcion.edad) raton }


--------Quitar los comentarios desde aquí hasta alcachofa
---hierbaVerde, elimina las enfermedades que terminen de cierta forma.
--Por ejemplo, si a cerebro le doy la hierbaVerde del tipo “sis”, queda sólo con sarampión.

---sacar este comentairio
hierbaVerde :: String -> Hierba 
hierbaVerde sufijo = cambiarEnfermedades (quitarEnfermedad sufijo) 

cambiarEnfermedades :: ([String]-> [String]) -> Hierba
cambiarEnfermedades funcion raton = raton {enfermedades = (funcion.enfermedades) raton}

quitarEnfermedad ::  String-> [String] ->[String]
quitarEnfermedad  sufijo listaEnfermedades   = filter (not.(coincide sufijo)) listaEnfermedades

coincide :: String -> String-> Bool
coincide sufijo laEnfermedad= reverse (obtenerSufijo laEnfermedad sufijo) == sufijo

obtenerSufijo :: String -> String -> String
obtenerSufijo laEnfermedad sufijo = take (length sufijo) (reverse laEnfermedad)



--alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
--Por ejemplo, un raton de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg. 
cambiarPeso :: (Float->Float) -> Hierba 
cambiarPeso funcion raton = raton {peso = (funcion.peso) raton}

alcachofa :: Hierba
alcachofa raton 
    | peso raton > 2 = cambiarPeso (*0.90) raton
    | otherwise = cambiarPeso (*0.95) raton

--hierbaZort, hace que el ratón se transforme en Pinky, perdiendo todas sus enfermedades y quedando con 0 años de edad.

----Opcion 1
{-hierbaZort :: Hierba
hierbaZort raton = pinky

pinky :: Raton
pinky = UnRaton "pinky" 0 _ []-}

---Opcion 2

hierbaZort :: Hierba
hierbaZort = cambiarEnfermedades quitarEnfermedades . cambiarEdad quitarEdad

quitarEdad :: Float -> Float
quitarEdad laEdad = 0

quitarEnfermedades ::  [String]-> [String]
quitarEnfermedades listaEnfermedades = [ ]
--hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0) y elimina todas las enfermedades con menos de 10 letras.

hierbaDelDiablo :: Hierba
hierbaDelDiablo  = cambiarPeso (+(-0.1)). cambiarEnfermedades menoresAdiez

menoresAdiez :: [String] -> [String] 
menoresAdiez  listaEnfermedades = filter (( <10).length) listaEnfermedades


-----Punto 4
{-Medicamentos: Los medicamentos son la administración sucesiva de un conjunto de hierbas. Se pide crear los siguientes 
medicamentos para luego poder administrarlos en un ratón: 
Hacer el pondsAntiAge, que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa. iterate
Por ejemplo, si se lo administramos al ratón Bicenterrata, queda con 2 años y 0.19 kg 
Hacer el reduceFatFast, (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde 
de “obesidad” y tantas alcachofas como indique su potencia.
Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg y sólo 
quede con sinusitis. Si en lugar de la 1 le administramos un reduceFatFast de potencia 2, pasa a pesar 8.1 kg y 
queda también solo con sinusitis.
Hacer la pdepCilina, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas. 
Las enfermedades infecciosas son aquellas cuyo nombre termina de alguna de estas formas (utilizar esta constante):
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]
-}

administrarMedicamento :: [Hierba] -> Raton -> Raton
administrarMedicamento listaHierbas raton = foldl aplicarHierba raton listaHierbas

aplicarHierba :: Raton -> Hierba -> Raton
aplicarHierba  raton hierba = hierba raton 

pondsAntiAge :: [Hierba]
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]


reduceFatFast ::  Int -> [Hierba]
reduceFatFast potencia = [hierbaVerde string] ++ crearReduceFatFast potencia

crearReduceFatFast :: Int -> [Hierba]
crearReduceFatFast 0 = [ ]
crearReduceFatFast potencia = [alcachofa] ++ crearReduceFatFast (potencia -1)

    







