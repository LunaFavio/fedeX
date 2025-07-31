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
    impuestos :: [String]
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

--IVA: 20% del precio
iva envio = aplicarImpuestoSi (const True) 20 envio


--Multicategoría: 1% del precio, se aplica cuando el envío tiene más de 3 categorías.
multicategoria cantCategorias = aplicarImpuestoSi ((>cantCategorias).length.categorias) 1

--Aduanero: 3%, pero sólo cuando el envío es internacional. 
--Un pedido es internacional cuando los países de origen y destino difieren.
aduanero envio = aplicarImpuestoSi esInternacional 3

--Impuesto extraño: 10%, sólo si tiene precio par.
extranio envio = aplicarImpuestoSi (even.precioBase) 10 envio

esInternacional envio = (pais.origen $ envio) /= (pais.destino $ envio)

aplicarImpuestoSi condicion porcentaje envio
    | condicion envio = aplicarImpuesto porcentaje envio
    | otherwise = envio

aplicarImpuesto porcentaje envio = envio {
    precioBase = precioBase envio + (precioBase envio * porcentaje)
}
