{--
Estados:
Para conocer cada situación que puede darse a lo largo de la búsqueda
de la solución del problema hay que conocer en qué orilla se
encuentra cada uno de los personajes implicados. Utilizaremos una
tupla de aridad cuatro. Cada uno de los elementos de dicha tupla se
corresponde con la orilla en la que se encuentra cada uno de los
personajes, considerados según el siguiente orden: granejro, lobo,
cabra y col. Llamaremos Izquierda a la orilla de partida y Derecha a
de llegada.
--}

data Orilla = Izquierda | Derecha
            deriving Eq
type Estado = (Orilla, Orilla, Orilla, Orilla)

{--
Para manejar dicha representación y que el código de los elementos
solicitados no dependa de la misma, creamos las siguientes funciones
auxiliares (las únicas que habría que modificar si decidimos
modificar la representación del problema).
--}

-- Dadas las orillas en las que se encuentran cada uno de los
-- personajes (en el orden indicado: granjero, lobo, cabra y col),
-- devuelve el estado correspondiente.
creaEstado :: Orilla -> Orilla -> Orilla -> Orilla -> Estado
creaEstado gr lb cb cl = (gr, lb, cb, cl)

-- Dado un estado, devuelven la orilla del personaje indicado.
orillaGranjero :: Estado -> Orilla
orillaGranjero (gr,_,_,_) = gr

orillaLobo :: Estado -> Orilla
orillaLobo (_,lb,_,_) = lb

orillaCabra :: Estado -> Orilla
orillaCabra (_,_,cb,_) = cb

orillaCol :: Estado -> Orilla
orillaCol (_,_,_,cl) = cl

-- Calcula, dada la orilla en la que se encuentra un personaje, a que
-- orilla llegaría si se traslada en la barca.
opuesta :: Orilla -> Orilla
opuesta Izquierda = Derecha
opuesta Derecha = Izquierda

{--
Movimientos:
Existen cuatro posibilidades, que el granjero cruce solo en la barca,
que cruce con el lobo, que cruce con la cabra o que cruce con la col.
Creamos un tipo enumerativo con un elemento para cada posibilidad
(fácilmente identificable por el nombre elegido).
--}

data Movimiento = Granjero | Lobo | Cabra | Col
                deriving Show

{--
Elementos necesarios para la representación del problema como espacio
de estados (incluidos algunos auxiliares para algunos de ellos.

Estados iniciales: en este caso sólo hay uno posible, aquel en el que
los cuatro personajes se encuentran en la orilla de partida (la
izquierda).
--}

estadoInicial :: Estado
estadoInicial =  creaEstado Izquierda Izquierda Izquierda Izquierda

{--
Función esEstadoFinal: al haber un único final posible, que todos los
personajes acaben en la orilla de llegada (la derecha) comparamos con
dicha situación.
Para poder comparar las orillas hemos añadido la clase Eq a la
definición del tipo Orilla.
--}

estadoFinal :: Estado
estadoFinal = creaEstado Derecha Derecha Derecha Derecha

esEstadoFinal :: Estado -> Bool
esEstadoFinal st = st == estadoFinal

{--
Función aplicables: la función auxiliar (con una ecuación por cada
posible movimiento) determina que condiciones deben darse para
poderlo utilizar.
--}

-- El granjero puede cambiar de orilla si no deja atrás a la cabra
-- con el lobo o a la cabra con la col.
esAplicable :: Movimiento -> Estado -> Bool
esAplicable Granjero st =
  ((orillaCabra st) /= (orillaLobo st) ||
   (orillaLobo st) /= (orillaGranjero st)) &&
  ((orillaCabra st) /= (orillaCol st) ||
   (orillaCol st) /= (orillaGranjero st))

-- El granjero se puede llevar al lobo si están juntos y no deja
-- atrás a la cabra con la col.
esAplicable Lobo st =
  (orillaLobo st == orillaGranjero st) &&
  ((orillaCabra st) /= (orillaCol st) ||
   (orillaCol st) /= (orillaGranjero st))

-- El granjero se puede llevar a la cabra si están juntos.
esAplicable Cabra st = (orillaCabra st == orillaGranjero st)

-- El granejro se puede llevar a la col si están juntos y no deja
-- atrás a cabra con el lobo.
esAplicable Col  st =
  (orillaCol st == orillaGranjero st) &&
  ((orillaCabra st) /= (orillaLobo st) ||
   (orillaLobo st) /= (orillaGranjero st))

-- Con la función anterior la definición de aplicables es inmediata.
aplicables :: Estado -> [Movimiento]
aplicables st = [mv | mv <- [Granjero, Lobo, Cabra, Col], esAplicable mv st]

{--
Función aplica:
Según el movimiento cambian de orilla unos personajes u otros, el
resto permanece en la misma orilla.
--}

-- Sólo el granjero
aplica :: Movimiento -> Estado -> Estado
aplica Granjero st =
  creaEstado (opuesta (orillaGranjero st)) (orillaLobo st)
 (orillaCabra st) (orillaCol st)

-- El granjero y el lobo.
aplica Lobo st =
  creaEstado (opuesta (orillaGranjero st)) (opuesta (orillaLobo st)) 
  (orillaCabra st) (orillaCol st)

-- El granjero y la cabra.
aplica Cabra st =
  creaEstado (opuesta (orillaGranjero st)) (orillaLobo st) 
  (opuesta (orillaCabra st)) (orillaCol st)

-- El granjero y la col.
aplica Col st =
  creaEstado (opuesta (orillaGranjero st)) (orillaLobo st)
  (orillaCabra st) (opuesta (orillaCol st))
