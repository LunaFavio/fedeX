module Library where
import PdePreludat


--MODELO PARCIAL FEDEX
--Nuestro amigo Fede tiene una empresa de envíos postales y nos solicita una pequeña aplicación 
--para administrar los pedidos que debe enviar a los distintos destinos en los cuales dicha 
--organización brinda servicios. Mientras nosotros lo programamos, él se va a quejar con los 
--que le pintaron mal el logo en aviones y camiones. Los envíos tienen distintas características 
--como información básica, éstas son: lugar de origen (ciudad y país), lugar de destino (idem), 
--peso en kilogramos, precio base, categorías que brindan información del contenido 
--(por ejemplo: tecnología, libro, música, mueble...etc), y varios impuestos asociados 
--(IVA, Aduanero, etc.).

--1) Crear el modelo necesario que mejor se adapte para la solución.
--Además:

data Envio = Envio {
    origen :: Lugar,
    destino :: Lugar,
    peso :: Number,
    precioBase :: Number,
    categorias :: [String],
    impuestos :: [Impuesto]
} deriving (Show, Eq)

type Lugar = (String, String)
ciudad = fst
pais = snd

--a) Indique el tipo de un cargo.
type Cargo = Envio -> Envio
--Cargo categórico: Si el envío tiene una categoría X, se computa un porcentaje dado del precio base.
categorico categoria porcentaje envio = aplicarCargoSi (elem categoria . categorias) ((porcentaje *).precioBase $ envio) envio

--Cargo por sobrepeso: Si el peso es menor o igual a un peso dado (en Kg.), no se afecta el precio. 
--                     En el caso de ser mayor se le suma $80 por cada kilo que lo supere.
sobrePeso pesoMax envio = aplicarCargoSi ((>= pesoMax). peso) ((80*) $ peso envio - pesoMax) envio
    --aumento2 = (80*).subtract pesoMax.peso $ envio

--Cargo arbitrario: $50 adicionales. Porque sí.
cargoArbitrario envio = aplicarCargoSi (const True) 50 envio
--Podría haber otros.

aplicarCargoSi condicion aumento envio
    | condicion envio = envio { precioBase = precioBase envio + aumento}
    | otherwise = envio

--b) Indique el tipo de un impuesto.Además, los impuestos, que aplican sobre el precio bruto 
--   (el precio base tras sumar todos los cargos extra). 
--   Algunos de los impuestos que pueden aplicarse son
type Impuesto = Envio -> Envio

--IVA: 20% del precio
iva envio = aplicarImpuestoSi (const True) 20 envio


--Multicategoría: 1% del precio, se aplica cuando el envío tiene más de 3 categorías.
multicategoria cantCategorias = aplicarImpuestoSi ((>cantCategorias).length.categorias) 1

--Aduanero: 3%, pero sólo cuando el envío es internacional. 
--Un pedido es internacional cuando los países de origen y destino difieren.
aduanero envio = aplicarImpuestoSi esInternacional 3

esInternacional envio = (pais.origen $ envio) /= (pais.destino $ envio)

--Impuesto extraño: 10%, sólo si tiene precio par.
extranio envio = aplicarImpuestoSi (even.precioBase) 10 envio



aplicarImpuestoSi condicion porcentaje envio
    | condicion envio = aplicarImpuesto porcentaje envio
    | otherwise = envio

aplicarImpuesto porcentaje envio = envio {
    precioBase = precioBase envio + (precioBase envio * porcentaje)
}

--2 Modelar con funciones constantes:
--a Un cargo categórico de “tecnología” de 18%.
categorico :: String -> Number -> Cargo
cargoTecnologia :: Cargo
cargoTecnologia = categorico "tecnologia" 18

--b Envío con origen en Buenos Aires, Argentina y con destino Utrecht, Países Bajos, de 2 kg. de peso, 
--  precio base de $220, con las categorías de música y tecnología, sin impuestos.
envioB = Envio {
    origen=("Buenos Aires","Argentina"), 
    destino=("Utrecht", "Paises Bajos"), 
    peso=2, 
    precioBase=220, 
    categorias=["musica","tecnologia"], 
    impuestos=[]
    }

--c Envío con origen California, Estados Unidos y con destino Miami, Estado Unidos, de 5 kg. de peso, 
--  precio base $1500, con categoría de libros, y con IVA e impuesto extraño.

envioC = Envio {
    origen=("California","Estados Unidos"), 
    destino=("Miami","Estados Unidos"), 
    peso=5, 
    precioBase=1500, 
    categorias=["Libros"], 
    impuestos=[iva, extranio]
    }

--3 Sobre el precio...
--a Saber si el precio base de un envío cuesta más que un valor determinado N.
cuestaMas precioComparacion (Envio _ _ _ precioBase _ _) = precioBase > precioComparacion

--b Conocer si un envío es barato. Decimos que es barato si vale $1300 o menos (precio base).
envioBarato = not.cuestaMas 1300 

--4 Sobre los lugares...
--a Saber si un envío se dirige a un país determinado.
seDirigeA unPais (Envio _ (c,p) _ _ _ _) = unPais == p
seDirigeA' unPais (Envio _ destino _ _ _ _) = unPais == pais destino

--b Dado un envío, determinar si es local o es internacional. Es local cuando los países de origen y de destino son iguales.
esLocal envio = seDirigeA (pais.origen $ envio) envio

esInternacional' = not.esLocal

--5 A partir de un conjunto de envíos, obtener aquellos que tienen ciertas categorías.
--  Nota: No se puede usar expresiones lambda, definiciones locales ni fuasdasnciones auxiliares.
-- si en las categorias del envio hay alguna de las categorias de la lista
tienenCategorias :: [String] -> [Envio] -> [Envio]
tienenCategorias listaCategorias envios = filter (any (`elem` listaCategorias ). categorias) envios

-- si todas las categorias del envio estan en la lista
tienenCategorias' listaCategorias envios = filter (all (`elem` listaCategorias ). categorias) envios

--6 Obtener el precio total de un envío, en base a los impuestos que tiene asignado y a un conjunto 
--  de cargos que se aplican en la sucursal de envío.
--  Mostrar un único ejemplo de consulta (no hace falta la respuesta) con un envío y una muestra de 
--  cada uno de los 3 ejemplos de cargos descriptos anteriormente.

--se aplican los impuestos una vez aplicados los cargos 
aplicarImpuestos :: [Cargo] -> Envio -> Envio
aplicarImpuestos cargos envio = 
    foldl (\envio impuesto -> impuesto envio) (foldl (\envio cargo -> cargo envio) envio cargos) impuestosAsignados
    where
        impuestosAsignados = impuestos envio
    
precioFinal cargosSucursal = precioBase.aplicarImpuestos cargosSucursal

--consulta: precioFinal [cargoTecnologia, cargoArbitrario, (categorico "musica" 10)] envioB
--consulta: precioFinal [cargoArbitrario, (categorico "libros" 10), (sobrePeso 1)] envioA

--7
--  Determinar, para un envío dado, el cargo más doloroso de una lista, que es aquel 
--  que produce un incremento de precio total mayor individualmente.

--cargoDoloroso :: [Cargo] -> Envio -> Cargo
cargoDoloroso cargos envio = foldl1 (cargoMayor envio) cargos

cargoMayor :: Envio -> Cargo -> Cargo -> Cargo
cargoMayor envio c1 c2
    | precioBase (c1 envio) > precioBase (c2 envio) = c1
    | otherwise = c2

--8
-- Explicar el tipo de la siguiente función:
-- whatever x y z  = z x . filter ((x==) . y) 
-- la funcion realiza un filtro sobre una lista y el resultado de ese filtro le aplica una nueva funcion
-- Lo primero que hace la funcion es un filtro sobre una lista. La condicion para filtrar es la siguiente:
-- a los elementos de la lista se les aplica y, el resultado es comparado con x y verifica si son iguales
-- Por ello x es un elemento de comparacion e y es una funcion. 
-- a la lista ya filtrada se le aplica la funcion z q recibe 2 parametros, el elemento x y la lista resultante
-- del filter
-- la funcion termina devolviendo lo que retonra la funcion z