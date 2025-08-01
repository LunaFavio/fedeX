module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

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