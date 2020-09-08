data Pieza = Ninguna | Verde | Azul | Roja | Naranja | Rosa | Amarilla deriving (Show, Eq)
data Casilla = Vacia | C (Double, Double) deriving (Show, Eq)

type Movimiento = (Pieza,Casilla)
type Estado = ([Pieza], Pieza, [Casilla], [Movimiento])

piezas :: [Pieza]
piezas = [Verde,Azul,Roja,Naranja,Rosa,Amarilla]

dimensionPieza :: Pieza -> [Casilla]
dimensionPieza Verde = [C (x, y)|x<-[0.0..3.0], y<-[0.0..1.0]]
dimensionPieza Azul = [C (x, y)|x<-[0.0..3.0], y<-[0.0..4.0]]
dimensionPieza Roja = [C (x, y)|x<-[0.0..2.0], y<-[0.0..7.0]]
dimensionPieza Naranja = [C (x, y)|x<-[0.0..6.0], y<-[0.0..2.0]]
dimensionPieza Rosa = [C (x, y)|x<-[0.0..2.0], y<-[0.0..6.0]]
dimensionPieza Amarilla = [C (x, y)|x<-[0.0..2.0], y<-[0.0..1.0]]

tablero :: [Casilla]
tablero = [C (((-5.0)+1/2)+x,((-5.0)+1/2)+y)|x<-[0.0..9.0], y<-[0.0..9.0]]

calculaOpciones :: [Casilla] -> Pieza -> [Movimiento]
calculaOpciones _ Ninguna = []
calculaOpciones recuadrosLibres piezaElegida = 
   [(piezaElegida, puntoPlano) | puntoPlano <- recuadrosLibres, (calculaOpcionesAux puntoPlano (dimensionPieza piezaElegida) recuadrosLibres)]

calculaOpcionesAux :: Casilla -> [Casilla] -> [Casilla] -> Bool
calculaOpcionesAux (C (x, y)) dimension recuadrosLibres = and[(C (x+k, y+z)) `elem` recuadrosLibres|(C (k, z)) <- dimension]

construirPieza :: Casilla -> [Casilla] -> [Casilla]
construirPieza (C (x, y)) dimension = [(C (x+k, y+z))|(C (k, z))<-dimension]

quitaPieza :: [Pieza] -> Pieza -> [Pieza]
quitaPieza [x] pieza = []
quitaPieza (x:xs) pieza
   |pieza == x = xs
   |otherwise = x:(quitaPieza xs pieza)
   
quitaRecuadrosLibres :: [Casilla] -> [Casilla] -> [Casilla]
quitaRecuadrosLibres recuadrosLibres [] = recuadrosLibres
quitaRecuadrosLibres recuadrosLibres (x:xs) = (quitaRecuadrosLibres (quitaRecuadrosLibresAux recuadrosLibres x) xs)

quitaRecuadrosLibresAux :: [Casilla] -> Casilla -> [Casilla]
quitaRecuadrosLibresAux [x] recuadroLibre = []
quitaRecuadrosLibresAux (x:xs) recuadroLibre 
   |x == recuadroLibre = xs
   |otherwise = x:(quitaRecuadrosLibresAux xs recuadroLibre)

calculaSiguientes :: Estado -> [[Movimiento]]
calculaSiguientes estado@([], _, [], secuencia) = [secuencia]
calculaSiguientes estado@(_, piezaElegida, recuadrosLibres, _) = 
   concat[(recursivo opcion estado)|opcion<-(calculaOpciones recuadrosLibres piezaElegida)]

recursivo :: Movimiento -> Estado -> [[Movimiento]]
recursivo (pieza, inicioPieza) estado@(piezasRestantes, _, recuadrosLibres, secuencia) 
   |length(nuevasPiezasRestantes)==0 = calculaSiguientes (nuevasPiezasRestantes, Ninguna, nuevosRecuadrosLibres, secuencia++[(pieza, inicioPieza)])
   |otherwise = calculaSiguientes (nuevasPiezasRestantes, nuevasPiezasRestantes!!0, nuevosRecuadrosLibres, secuencia++[(pieza, inicioPieza)])
   where nuevasPiezasRestantes = (quitaPieza piezasRestantes pieza)
         nuevosRecuadrosLibres = quitaRecuadrosLibres recuadrosLibres (construirPieza inicioPieza (dimensionPieza pieza))

{--
calculaSiguientes estadoInicial1

[[(Ninguna,Vacia),(Verde,C (-4.5,-4.5)),(Azul,C (-4.5,-2.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-0.5,-4.5)),(Amarilla,C (2.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,-4.5)),(Azul,C (-4.5,-2.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-0.5,-4.5)),(Amarilla,C (2.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,-4.5)),(Azul,C (-1.5,-2.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (2.5,-4.5)),(Amarilla,C (-0.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,-1.5)),(Azul,C (-4.5,0.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-0.5,-1.5)),(Amarilla,C (2.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,-1.5)),(Azul,C (-4.5,0.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-0.5,-1.5)),(Amarilla,C (2.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,0.5)),(Azul,C (-4.5,-4.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-0.5,-4.5)),(Amarilla,C (2.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,0.5)),(Azul,C (-4.5,-4.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-0.5,-4.5)),(Amarilla,C (2.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,3.5)),(Azul,C (-4.5,-1.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-0.5,-1.5)),(Amarilla,C (2.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,3.5)),(Azul,C (-4.5,-1.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-0.5,-1.5)),(Amarilla,C (2.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-4.5,3.5)),(Azul,C (-1.5,-1.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (2.5,-1.5)),(Amarilla,C (-0.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,-4.5)),(Azul,C (-1.5,-2.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (2.5,-4.5)),(Amarilla,C (-4.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,-4.5)),(Azul,C (-1.5,-2.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (2.5,-4.5)),(Amarilla,C (-4.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,-4.5)),(Azul,C (-1.5,-2.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-4.5,-4.5)),(Amarilla,C (2.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,-4.5)),(Azul,C (-1.5,-2.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-4.5,-4.5)),(Amarilla,C (2.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,-1.5)),(Azul,C (-1.5,0.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (2.5,-1.5)),(Amarilla,C (-4.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,-1.5)),(Azul,C (-1.5,0.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (2.5,-1.5)),(Amarilla,C (-4.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,-1.5)),(Azul,C (-1.5,0.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-4.5,-1.5)),(Amarilla,C (2.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,-1.5)),(Azul,C (-1.5,0.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-4.5,-1.5)),(Amarilla,C (2.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,0.5)),(Azul,C (-1.5,-4.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (2.5,-4.5)),(Amarilla,C (-4.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,0.5)),(Azul,C (-1.5,-4.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (2.5,-4.5)),(Amarilla,C (-4.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,0.5)),(Azul,C (-1.5,-4.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-4.5,-4.5)),(Amarilla,C (2.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,0.5)),(Azul,C (-1.5,-4.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-4.5,-4.5)),(Amarilla,C (2.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,3.5)),(Azul,C (-1.5,-1.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (2.5,-1.5)),(Amarilla,C (-4.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,3.5)),(Azul,C (-1.5,-1.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (2.5,-1.5)),(Amarilla,C (-4.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,3.5)),(Azul,C (-1.5,-1.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-4.5,-1.5)),(Amarilla,C (2.5,3.5))],
[(Ninguna,Vacia),(Verde,C (-1.5,3.5)),(Azul,C (-1.5,-1.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-4.5,-1.5)),(Amarilla,C (2.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (1.5,-4.5)),(Azul,C (-1.5,-2.5)),(Roja,C (2.5,-2.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-4.5,-4.5)),(Amarilla,C (-1.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (1.5,-4.5)),(Azul,C (1.5,-2.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (-1.5,-4.5)),(Amarilla,C (-4.5,3.5))],
[(Ninguna,Vacia),(Verde,C (1.5,-4.5)),(Azul,C (1.5,-2.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (-1.5,-4.5)),(Amarilla,C (-4.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (1.5,-1.5)),(Azul,C (1.5,0.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (-1.5,-1.5)),(Amarilla,C (-4.5,3.5))],
[(Ninguna,Vacia),(Verde,C (1.5,-1.5)),(Azul,C (1.5,0.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (-1.5,-1.5)),(Amarilla,C (-4.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (1.5,0.5)),(Azul,C (1.5,-4.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (-1.5,-4.5)),(Amarilla,C (-4.5,3.5))],
[(Ninguna,Vacia),(Verde,C (1.5,0.5)),(Azul,C (1.5,-4.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (-1.5,-4.5)),(Amarilla,C (-4.5,-4.5))],
[(Ninguna,Vacia),(Verde,C (1.5,3.5)),(Azul,C (-1.5,-1.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,-4.5)),(Rosa,C (-4.5,-1.5)),(Amarilla,C (-1.5,3.5))],
[(Ninguna,Vacia),(Verde,C (1.5,3.5)),(Azul,C (1.5,-1.5)),(Roja,C (-4.5,-4.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (-1.5,-1.5)),(Amarilla,C (-4.5,3.5))],
[(Ninguna,Vacia),(Verde,C (1.5,3.5)),(Azul,C (1.5,-1.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (-1.5,-1.5)),(Amarilla,C (-4.5,-4.5))]]

length(calculaSiguientes estadoInicial1)

36
--}

estadoInicial1 :: Estado
estadoInicial1 = (piezas, piezas!!0, tablero, [(Ninguna, Vacia)])

estadoInicial2 :: Estado
estadoInicial2 = (piezas, piezas!!1, tablero, [(Ninguna, Vacia)])

estadoInicial3 :: Estado
estadoInicial3 = (piezas, piezas!!2, tablero, [(Ninguna, Vacia)])

estadoInicial4 :: Estado
estadoInicial4 = (piezas, piezas!!3, tablero, [(Ninguna, Vacia)])

estadoInicial5 :: Estado
estadoInicial5 = (piezas, piezas!!4, tablero, [(Ninguna, Vacia)])

estadoInicial6 :: Estado
estadoInicial6 = (piezas, piezas!!5, tablero, [(Ninguna, Vacia)])

esEstadoFinal :: Estado -> Bool
esEstadoFinal estado@([], _, [], _) = True
esEstadoFinal _ = False

aplicables :: Estado -> [Movimiento]
aplicables (_, pieza, recuadrosLibres, _) = (calculaOpciones recuadrosLibres pieza)

aplica :: Movimiento -> Estado -> Estado
aplica (pieza, inicioPieza) (piezasRestantes, _, recuadrosLibres, secuencia)
   |length(nuevasPiezasRestantes) == 0 = (nuevasPiezasRestantes, Ninguna, nuevosRecuadrosLibres, secuencia++[(pieza, inicioPieza)])
   |otherwise = (nuevasPiezasRestantes, nuevasPiezasRestantes!!0, nuevosRecuadrosLibres, secuencia++[(pieza, inicioPieza)])
   where nuevasPiezasRestantes = (quitaPieza piezasRestantes pieza)
         nuevosRecuadrosLibres = quitaRecuadrosLibres recuadrosLibres (construirPieza inicioPieza (dimensionPieza pieza))

busquedaAnchura :: Estado -> Maybe [Movimiento]
busquedaAnchura estado = metodo1 [] [estado]

{--
*Main> busquedaAnchura estadoInicial1
Just [(Ninguna,Vacia),(Verde,C (-4.5,-4.5)),(Azul,C (-4.5,-2.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-0.5,-4.5)),(Amarilla,C (2.5,3.5))]

*Main> busquedaAnchura estadoInicial2
Just [(Ninguna,Vacia),(Azul,C (-4.5,-4.5)),(Verde,C (-4.5,0.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,2.5)),(Rosa,C (-0.5,-4.5)),(Amarilla,C (2.5,3.5))]

*Main> busquedaAnchura estadoInicial3
Just [(Ninguna,Vacia),(Roja,C (-4.5,-4.5)),(Verde,C (-4.5,3.5)),(Azul,C (-1.5,-1.5)),(Naranja,C (-1.5,-4.5)),(Rosa,C (2.5,-1.5)),(Amarilla,C (-0.5,3.5))]

*Main> busquedaAnchura estadoInicial4
Just [(Ninguna,Vacia),(Naranja,C (-4.5,-4.5)),(Verde,C (-4.5,-1.5)),(Azul,C (-4.5,0.5)),(Roja,C (2.5,-4.5)),(Rosa,C (-0.5,-1.5)),(Amarilla,C (2.5,3.5))]

*Main> busquedaAnchura estadoInicial5
Just [(Ninguna,Vacia),(Rosa,C (-4.5,-4.5)),(Verde,C (-1.5,-4.5)),(Azul,C (-1.5,-2.5)),(Roja,C (2.5,-4.5)),(Naranja,C (-4.5,2.5)),(Amarilla,C (2.5,3.5))]

*Main> busquedaAnchura estadoInicial6
Just [(Ninguna,Vacia),(Amarilla,C (-4.5,-4.5)),(Verde,C (-1.5,-4.5)),(Azul,C (-1.5,-2.5)),(Roja,C (-4.5,-2.5)),(Naranja,C (-1.5,2.5)),(Rosa,C (2.5,-4.5))]

--}

metodo1 :: [Estado] -> [Estado] -> Maybe [Movimiento]
metodo1 explorados frontera
  |length(frontera) == 0 = Nothing
  |otherwise = metodo2 (frontera!!0) (explorados) (tail frontera)

metodo2 :: Estado -> [Estado] -> [Estado] -> Maybe [Movimiento]
metodo2 estado@(_, _, _, sucesion) explorados frontera
   |esEstadoFinal estado == True = Just sucesion
   |otherwise = metodo1 (explorados++[estado]) (anyadirSucesoresFrontera estado explorados frontera)

anyadirSucesoresFrontera :: Estado -> [Estado] -> [Estado] -> [Estado]
anyadirSucesoresFrontera estado explorados frontera = 
   anyadirFrontera [aplica opcion estado|opcion<-(aplicables estado)] explorados frontera

anyadirFrontera :: [Estado] -> [Estado] -> [Estado] -> [Estado]
anyadirFrontera [] explorados frontera = frontera
anyadirFrontera (x:xs) explorados frontera
   |(not(x `elem` explorados)) && (not(x `elem` frontera)) == True = (anyadirFrontera xs explorados (frontera++[x]))
   |otherwise = frontera
