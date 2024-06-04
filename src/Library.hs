module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Palabra = String
type Verso = String    -- frases con sentido
type Estrofa = [Verso] -- agrupacion de versos
type Artista = String  -- porque solamente nos interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)
--cumplen f comp v1 v2 = (f v1) `comp` (f v2)

-- 1) Se pide
-- A) Determinar si dos palabras riman. Es decir, si generan una rima, ya sea asonante o consonante, 
-- pero teniendo en cuenta que dos palabras iguales no se consideran una rima.

riman :: Palabra -> Palabra -> Bool
riman palabra1 palabra2 
    | sonPalabrasIguales palabra1 palabra2 = False                                 -- dos palabras iguales NO se consideran una rima!!
    | rimaAsonante' palabra1 palabra2 || rimaConsonante' palabra1 palabra2 = True  -- rimaAsonante o rimaConsonante si son rimas
    | otherwise = False    

rimanV2 :: Palabra -> Palabra -> Bool
rimanV2 palabra1 palabra2 = palabra1 /= palabra2 && (rimaAsonante' palabra1 palabra2 || rimaConsonante' palabra1 palabra2)                                         

sonPalabrasIguales :: Palabra -> Palabra -> Bool 
sonPalabrasIguales palabra1 palabra2 = palabra1 == palabra2

-- Rima asonante: se cumple cuando las dos últimas vocales de la palabra coinciden. Por ejemplo: parcial - estirar

--rimaAsonante :: Palabra -> Palabra -> Bool
--rimaAsonante palabra1 palabra2 = dosUltimasVocales palabra1 == dosUltimasVocales palabra2

rimaAsonante' :: Palabra -> Palabra -> Bool         -- forma mas corta usando la funcion cumple!!
rimaAsonante' = cumplen (ultimasVocales 2) (==)

ultimasLetras :: Number -> Palabra -> Palabra       -- esta funcion es la generalizacion de las demas!!
ultimasLetras n = reverse . take n . reverse

dosUltimasVocales :: Palabra -> Palabra             -- 2dos) agarro la lista de las vocales, la doy vuelta y agarro los primeros 2 elementos
dosUltimasVocales = ultimasLetras 2 . soloVocales

ultimasVocales :: Number -> Palabra -> Palabra
ultimasVocales n = ultimasLetras n . soloVocales

soloVocales :: Palabra -> Palabra                   -- 1ero) hago una lista con solo las vocales de una palabra
soloVocales palabra = filter (esVocalOTieneTilde) palabra

esVocalOTieneTilde :: Char -> Bool
esVocalOTieneTilde letra = esVocal letra || tieneTilde letra

-- Rima consonante: se cumple cuando las tres últimas letras de la palabra coinciden. Por ejemplo: función - canción

--rimaConsonante :: Palabra -> Palabra -> Bool
--rimaConsonante palabra1 palabra2 = tresUlstimasPalabras palabra1 == tresUlstimasPalabras palabra2

rimaConsonante' :: Palabra -> Palabra -> Bool
rimaConsonante' = cumplen (ultimasLetras 3) (==)      -- forma mas corta usando la funcion cumple!!

tresUlstimasPalabras :: Palabra -> Palabra
tresUlstimasPalabras = reverse . take 3 . reverse 

-- Enumerar todos los casos de test representativos (clases de equivalencia) de la función anterior. 
-- No hace falta escribir los tests (serían sólo sus nombres).
{-
Las clases de equivalencia son
  - Dos palabras riman por rima asonante
  - Dos palabras riman por rima consonante
  - Dos palabras iguales no riman
  - Dos palabras sin conexion no riman
-}
-- riman "hola" "hola"                      -- riman "función" "canción"
-- > False                                  -- > True

-- riman "parcial" "estirar"                -- riman "eze" "marcela"
-- > True                                   -- > False

-- 2) Modelar las conjugaciones anteriores. Tener en cuenta que debe ser sencillo agregar más conjugaciones al sistema,
-- ya que se planea hacerlo en próximas iteraciones.
-- Tip: Haskell tiene la función words que dado un string lo separa por espacios. Ejemplo:
-- > words "hola soy pepita"
-- ["hola","soy","pepita"]

-- A) Por medio de rimas: dos versos se conjugan con rima cuando logran rimar las últimas palabras de cada uno. Por ejemplo:
-- "no hace falta un programa que genere una canción"
-- "para saber que esto se resuelve con una función"

type Conjugacion = Verso -> Verso -> Bool  -- dos versos estan conjugados cuando...

--porMedioDeRimas :: Verso -> Verso -> Bool
--porMedioDeRimas verso1 verso2 = rimanV2 (agarrarUltimaPalabra verso1) (agarrarUltimaPalabra verso2)

porMedioDeRimas' :: Conjugacion
porMedioDeRimas' = cumplen agarrarUltimaPalabra rimanV2

agarrarUltimaPalabra :: Verso -> Palabra
agarrarUltimaPalabra = last . words

-- B) Haciendo anadiplosis: sucede cuando el segundo verso comienza con la misma palabra con la que termina el primero. Por ejemplo:
-- "este examen no se aprueba sin aplicación parcial"
-- "parcial lindo y divertido si rendiste todas las katas"

haciendoAnadiplosis :: Verso -> Verso -> Bool
haciendoAnadiplosis verso1 verso2 = sonPalabrasIguales (agarrarUltimaPalabra verso1) (agarrarPrimeraPalabra verso2)

haciendoAnadiplosis' :: Conjugacion
haciendoAnadiplosis' verso1 verso2 = agarrarUltimaPalabra verso1 == agarrarPrimeraPalabra verso2 

agarrarPrimeraPalabra :: Verso -> Palabra
agarrarPrimeraPalabra = head . words

-- 3) Se pide
-- A) Modelar los patrones

-- Un patrón es una forma de articular los versos dentro de una estrofa y existen muchas formas posibles de hacer esto,

type Patron = Estrofa -> Bool

-- describiremos las siguientes:

-- Simple: es un patrón en el que riman 2 versos, especificados por su posición en la estrofa. (riman dos versos cualesquiera)
-- Por ejemplo, la siguiente estrofa tiene un patrón simple de 1 y 4, pero no de 1 y 3:
versitito1 :: Verso 
versitito1 = "esta rima es fácil como patear un penal"              -- (1) esta rima es fácil como patear un penal
versitito2 :: Verso 
versitito2 = "solamente tiene como objetivo servir de ejemplo"      -- (2) solamente tiene como objetivo servir de ejemplo
versitito3 :: Verso 
versitito3 = "los versos del medio son medio fríos"                 -- (3) los versos del medio son medio fríos
versitito4 :: Verso 
versitito4 = "porque el remate se retoma al final"                  -- (4) porque el remate se retoma al final

estrofitita :: Estrofa
estrofitita = [versitito1,versitito2,versitito3,versitito4]

type Par = (Number, Number)   -- un par de versos cualesquiera

--simple' :: Verso -> Verso -> Verso -> Verso -> Bool
--simple' verso1 verso2 verso3 verso4 = porMedioDeRimas verso1 verso4

simple :: Par -> Patron             -- Par -> (Estrofa -> Bool)
simple (n1,n2) estrofa = versoEsp n1 estrofa `porMedioDeRimas'` versoEsp n2 estrofa

versoEsp :: Number -> Estrofa -> Verso
versoEsp n estrofa = estrofa !! (n-1)   -- (n-1) porque si quiero el verso 4 de la estrofa, esta en la posicion 3 de la la estrofa (estrofa = lista de versos)

-- Esdrújulas: Todos los versos terminan con palabras en esdrújula. 
-- Diremos que una palabra es esdrújula cuando la antepenúltima vocal está acentuada. Un ejemplo de este patrón sería:
versito1 :: Verso
versito1 = "a ponerse los guantes y subir al cuadrilátero"            -- (1) a ponerse los guantes y subir al cuadrilátero
versito2 :: Verso
versito2 = "que después de este parcial acerca el paradigma lógico"   -- (2) que después de este parcial acerca el paradigma lógico
versito3 :: Verso
versito3 = "no entiendo por qué está fallando mi código"              -- (3) no entiendo por qué está fallando mi código
versito4 :: Verso
versito4 = "si todas estas frases terminan en esdrújulas"             -- (4) si todas estas frases terminan en esdrújulas

estrofita :: Estrofa
estrofita = [versito1,versito2,versito3,versito4]

esdrujulas :: Verso -> Verso -> Verso -> Verso -> Bool  
esdrujulas verso1 verso2 verso3 verso4 = esEsdrujula verso1 && esEsdrujula verso2 && esEsdrujula verso3 && esEsdrujula verso4

esdrujulasV2 :: Patron
esdrujulasV2 = all (esEsdrujulaV2 . agarrarUltimaPalabra)       -- forma mas facil!!

esEsdrujula :: Palabra -> Bool                                  -- 1ero) hago una lista con solo las vocales de la palabra 
esEsdrujula = tieneTilde . agarrarTerceraLetra . soloVocales    -- 2dos) agarro la antepenultima letra de la palabra
                                                                -- 3ero) me fijo si tiene tilde (esta acentuada la letra) 
esEsdrujulaV2 :: Palabra -> Bool
esEsdrujulaV2 = tieneTilde . head . ultimasVocales 3            -- forma mas facil!!

agarrarTerceraLetra :: Palabra -> Char -- agarro la antepenultima letra de la palabra (la 3era de atras para adelante)
agarrarTerceraLetra palabra =  (reverse palabra) !! 2 

-- agarrarTerceraLetra (soloVocales "cuadrilátero")
-- > '\225'    ------> En ASCII extendido (ISO-8859-1 o Latin-1), 225 corresponde a la letra 'á'. 

-- Anáfora: Todos los versos comienzan con la misma palabra. Por ejemplo:
versote1 :: Verso 
versote1 = "paradigmas hay varios, recién vamos por funcional"     -- (1) paradigmas hay varios, recién vamos por funcional
versote2 :: Verso 
versote2 = "paradigmas de programación es lo que analizamos acá"   -- (2) paradigmas de programación es lo que analizamos acá
versote3 :: Verso 
versote3 = "paradigmas que te invitan a otras formas de razonar"   -- (3) paradigmas que te invitan a otras formas de razonar
versote4 :: Verso 
versote4 = "paradigmas es la materia que más me gusta cursar"      -- (4) paradigmas es la materia que más me gusta cursar

estrofata :: Estrofa
estrofata = [versote1,versote2,versote3,versote3]

anafora :: Patron
anafora = iguales . map agarrarPrimeraPalabra

iguales :: [Palabra] -> Bool
iguales [] = False
-- iguales (palabra1:palabra2:palabras) = palabra1 == palabra2 && iguales (palabra2:palabras)
iguales (palabra1:palabras) = all (==palabra1) palabras

--anafora' :: Verso -> Estrofa -> Bool
--anafora' verso1 versos = all (sonPalabrasIguales (agarrarPrimeraPalabra verso1)) (agarrarPrimeraPalabra versos)

--anafora :: Verso -> Verso -> Verso -> Verso -> Bool
--anafora verso1 verso2 verso3 verso4 = agarrarPrimeraPalabra verso1 == agarrarPrimeraPalabra verso2 == agarrarPrimeraPalabra verso3 == agarrarPrimeraPalabra verso4

-- Cadena: Es un patrón que se crea al conjugar cada verso con el siguiente, usando siempre la misma conjugación. 
-- La conjugación usada es elegida por el artista mientras está rapeando. Por ejemplo, una cadena de anadiplosis sería:
-- (1) este es un ejemplo de un parcial compuesto
-- (2) compuesto de funciones que se operan entre ellas
-- (3) ellas también pueden pasarse por parámetro
-- (4) parámetro que recibe otra función de alto orden

-- Tip: puede hacerse utilizando recursividad.

cadena :: Conjugacion -> Patron     -- (Verso -> Verso -> Bool) -> (Estrofa -> Bool)
cadena _ [] = False  -- si no tengo una estrofa vacia devuelve False
cadena _ [_] = True  -- si tengo una estrofa con solo un verso devuelve True
cadena conjugacion (verso1 : verso2 : versos) = conjugacion verso1 verso2 && cadena conjugacion (verso2 : versos)

-- CombinaDos: Dos patrones cualesquiera se pueden combinar para crear un patrón más complejo, y 
-- decimos que una estrofa lo cumple cuando cumple ambos patrones a la vez. 
-- Por ejemplo, si contemplamos el patrón combinado de esdrújulas y anáfora, una estrofa que cumpliría podría ser:
-- (1) estrofa que sirve como caso ejémplico
-- (2) estrofa dedicada a la gente fanática
-- (3) estrofa comenzada toda con anáfora
-- (4) estrofa que termina siempre con esdrújulas

combinaDos :: Patron -> Patron -> Patron
combinaDos patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa   -- es decir que cumple la estrofa cumble ambos patrones

-- B) Usar el punto a para definir los siguientes patrones combinados:
---  * aabb = patrón simple entre 1 y 2 + otro simple entre 3 y 4
---  * abab = patrón simple entre 1 y 3 + otro simple entre 2 y 4
---  * abba = patrón simple entre 1 y 4 + otro simple entre 2 y 3
--   * hardcore = patrón de cadena de rimas + esdrújulas

---  * aabb = patrón simple entre 1 y 2 + otro simple entre 3 y 4  ("rima el 1 verso con el 2 y rima el 3 con el 4")
aabb :: Patron
aabb = simple (1,2) `combinaDos` simple (3,4)

---  * abab = patrón simple entre 1 y 3 + otro simple entre 2 y 4  ("rima el 1 verso con el 3 y rima el 2 con el 4")
abab :: Patron
abab = simple (1,3) `combinaDos` simple (2,4)

---  * abba = patrón simple entre 1 y 4 + otro simple entre 2 y 3  ("rima el 1 verso con el 4 y rima el 2 con el 3")
abba :: Patron
abba = simple (1,4) `combinaDos` simple (2,3)

--   * hardcore = patrón de cadena de rimas + esdrújulas  ("")
hardcore :: Patron
hardcore = cadena porMedioDeRimas' `combinaDos` esdrujulasV2

-- C)¿Se podría saber si una estrofa con infinitos versos cumple con el patrón hardcore?

estrofaInfinita :: Estrofa
estrofaInfinita = repeat "holi"

-- Se podria saber que una estrofa con infinitos versos NO cumpla con el patron hardcore, es decir, devueva FALSE
-- La razon es porque ya con que uno de los patrones de hardcore NO se cumpla, Haskell no seguira avanzando en la lista inifinita de versos ya que cuenta con una evaluacion del tipo perezosa (lazy evaluation). 
-- Entonces, por ejemplo si los dos primeros dos versos NO conjugan por medio de rimas, entonces NO es necesario seguir buscando en la lista
-- Otro ejemplo seria, si el primer verso tiene su ultima palabra que NO es esdrujula, entonces la estrofa no cumple con el patron esdrujulas (ya si un verso no cumple, entonces TODOS no cumplen)

-- ¿Y el aabb? Justifique en cada caso específicamente por qué (no valen respuestas genéricas).
-- Ocurre algo similar que en el caso anterior, me puede devolver un FALSE si simplemente encuentra que el 1 verso NO rima con el 2 verso. 
-- Pero tambien me puedo devolver TRUE, porque aunque tenga una estrofa infinita, yo solo quiero evaluar si riman el verso 1 con el verso 2 y el verso 3 con el verso 4, entonces luego de comprobar eso
-- los demas versos que me sobran de la estrofa infinita NO seran necesarios evaluarlos (todo esto gracias a la evaluacion perezosa que tiene Haskell)

-- 4) Hacer que un artista se tire un freestyle a partir de la estrofa que quiere decir y 
-- el estilo que le quiera dar a su puesta en escena. 
-- Para ello se parte siempre de una puesta base que tiene potencia 1 y el público tranquilo, 
-- la que luego varía según el estilo utilizado.
-- El resultado de que un artista se tire un freestyle es una puesta en escena.

-- Por ahora pudimos identificar las siguientes variables significativas de una puesta en escena: 
-- si el público está exaltado o no, la potencia (un número), además de, claro está, 
-- la estrofa del freestyle (una sola, la puesta es por estrofa) y el artista.

data PuestaEnEscena = UnaPuestaEnEscena {
    artista :: Artista,
    publicoExaltado :: Bool,
    potencia :: Number,
    freestyle :: Estrofa
}deriving Show

puestaBase :: Artista -> Estrofa -> PuestaEnEscena
puestaBase mc estrofa = UnaPuestaEnEscena {artista = mc, freestyle = estrofa, potencia = 1, publicoExaltado = False}

modificacionDePotenciaSegunPorcentaje :: Number -> PuestaEnEscena -> PuestaEnEscena
modificacionDePotenciaSegunPorcentaje porcentaje puesta = puesta {potencia = ((100 + porcentaje)* potencia puesta) `div`100 } 

exaltarPublico :: Bool -> PuestaEnEscena -> PuestaEnEscena
exaltarPublico exaltado puesta = puesta {publicoExaltado = exaltado}

exaltarPublicoSiCumple :: Patron -> PuestaEnEscena -> PuestaEnEscena
exaltarPublicoSiCumple patron puesta = exaltarPublico (cumplePatron puesta patron) puesta 

cumplePatron :: PuestaEnEscena -> Patron -> Bool          -- si el artista puede lograr algun patron en particular con la estrofa del freestyle de la puesta en escena
cumplePatron puesta patron = patron (freestyle puesta)

-- Además nos dimos cuenta que en cada puesta en escena cada artista utiliza un estilo distinto, 
-- dependiendo del mensaje que quiere transmitir, que altera la puesta en escena original. Identificamos los siguientes casos:
-- Gritar: aumenta la potencia en un 50%
-- Responder un acote: conociendo su efectividad, aumenta la potencia en un 20%, y además el público queda exaltado si la respuesta fue efectiva, sino no lo queda.
-- Tirar técnicas: se refiere a cuando el artista deja en evidencia que puede lograr algún patrón en particular, aumenta la potencia en un 10%, además el público se exalta si la estrofa cumple con dicho patrón, sino no.

type Estilo = PuestaEnEscena -> PuestaEnEscena

gritar :: Estilo
gritar = modificacionDePotenciaSegunPorcentaje 50

responderUnAcote :: Bool -> Estilo
responderUnAcote efectiva = exaltarPublico efectiva . modificacionDePotenciaSegunPorcentaje 20

tirarTecnicas :: Patron -> Estilo
tirarTecnicas patron =  exaltarPublicoSiCumple patron . modificacionDePotenciaSegunPorcentaje 10

tirarfreestyle :: Artista -> Estrofa -> Estilo -> PuestaEnEscena 
tirarfreestyle artista estrofa estilo = estilo (puestaBase artista estrofa)
