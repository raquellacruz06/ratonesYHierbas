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
quitarEdad _ = 0

quitarEnfermedades ::  [String]-> [String]
quitarEnfermedades _ = [ ]
--hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0) y elimina todas las enfermedades con menos de 10 letras.

hierbaDelDiablo :: Hierba
hierbaDelDiablo  = cambiarPeso (+(-0.1)). cambiarEnfermedades menoresAdiez

menoresAdiez :: [String] -> [String] 
menoresAdiez  listaEnfermedades = filter (( <10).length) listaEnfermedades


-----Punto 3
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

-}

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

type Medicamento = [Hierba]
administrarMedicamento :: [Hierba] -> Raton -> Raton
administrarMedicamento listaHierbas raton = foldl aplicarHierba raton listaHierbas

aplicarHierba :: Raton -> Hierba -> Raton
aplicarHierba  raton hierba = hierba raton 

pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]


reduceFatFast ::   Int -> Medicamento
reduceFatFast potencia = [hierbaVerde "obesidad"] ++ crearReduceFatFast potencia


crearReduceFatFast :: Int -> Medicamento
crearReduceFatFast 0 = [ ]
crearReduceFatFast potencia = [alcachofa] ++ crearReduceFatFast (potencia -1)

-------Falta hacer pdepCilina

pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas


--------Punto 4
{-Experimento: Los laboratorios antes de publicar un medicamento, lo prueban con distintos ratones para evaluar los 
resultados:
Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural que 
la cumple.
> cantidadIdeal even           > cantidadIdeal (>5)
2                              6
-}

cantidadIdeal ::  (Int -> Bool) -> [Int]-> Int
cantidadIdeal condicion numeros = head (filter condicion numeros)

listaNumeros :: Int -> [Int]
listaNumeros num = num : (listaNumeros (num + 1))

numeros :: [Int]
numeros = listaNumeros 0

{-Saber si un medicamento lograEstabilizar una comunidad de ratones. Esto sucede cuando, luego de aplicarle el 
medicamento a todos los ratones de la comunidad, se elimina el sobrepeso y todos tienen menos de 3 enfermedades.
 Un ratón tiene sobrepeso si pesa más de 1kg.-}

lograEstabilizar :: Medicamento-> [Raton] -> Bool
lograEstabilizar medicamento comunidad = todosCumplen sinSobrepeso medicamento comunidad && todosCumplen menosDeTresEnfermedades medicamento comunidad

todosCumplen ::  (Raton-> Bool) -> Medicamento -> [Raton]-> Bool 
todosCumplen condicion medicamento comunidad = all condicion  (map (administrarMedicamento medicamento) comunidad)

sinSobrepeso :: Raton -> Bool
sinSobrepeso  = ( < 1).peso 

menosDeTresEnfermedades :: Raton-> Bool
menosDeTresEnfermedades = (<3).length.enfermedades

{-Diseñar el siguiente experimento: dado una comunidad de ratones, encontrar la potencia ideal del reduceFatFast 
necesaria para estabilizar la comunidad.-}

{-encontrarPotencia :: [Raton] -> Medicamento -> Int
encontrarPotencia comunidad reduceFatFast = calcularPotencia comunidad reduceFatFast potencia

calcularPotencia :: [Raton] -> Medicamento -> Int
calcularPotencia comunidad potencia | lograEstabilizar (reduceFatFast potencia) comunidad = potencia
                                    | otherwise = calcularPotencia comunidad (potencia+1)-}




{-Queremos saber si un medicamento logra estabilizar una comunidad infinita. ¿Podemos saberlo? Responder en estos 
dos casos:
Si todos los ratones quedan con menos de 1kg y sin enfermedades. Justificar.
No lo podemos saber porque necesita la información de todos antes de arrojar un resultado (eager evaluation), por lo tanto, al ser
una lista infinita nunca obtendrá tal resultado

Si un ratón queda con 2kg y 4 enfermedades. Justificar.
Si algún ratón llega a cumplir tal condición es posible obtenerlo utilizando funciones como 
head (filter condicionDada listaInfinita) ya que gracias a Lazy evaluation a head no le importa qué pasa con el resto de la lista,
cuando consiga el primero que cumpla la condición lo arroja 
-}







