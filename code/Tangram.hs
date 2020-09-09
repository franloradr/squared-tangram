{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

data Pieza = Ninguna | Verde | Azul | Roja | Naranja | Rosa | Amarilla deriving (Show,Eq)

type Casilla = (Double, Double)
type Mundo = ([Pieza], Pieza, [Casilla], [Casilla], [Casilla], [Casilla], [Casilla], [Casilla], [Casilla])

piezas :: [Pieza]
piezas = [Verde,Azul,Roja,Naranja,Rosa,Amarilla]

dimensionPieza :: Pieza -> [Casilla]
dimensionPieza Verde = [(x, y)|x<-[0.0..3.0], y<-[0.0..1.0]]
dimensionPieza Azul = [(x, y)|x<-[0.0..3.0], y<-[0.0..4.0]]
dimensionPieza Roja = [(x, y)|x<-[0.0..2.0], y<-[0.0..7.0]]
dimensionPieza Naranja = [(x, y)|x<-[0.0..6.0], y<-[0.0..2.0]]
dimensionPieza Rosa = [(x, y)|x<-[0.0..2.0], y<-[0.0..6.0]]
dimensionPieza Amarilla = [(x, y)|x<-[0.0..2.0], y<-[0.0..1.0]]

{--
dimensionPieza Verde

[(0.0,0.0),(0.0,1.0),(1.0,0.0),(1.0,1.0),(2.0,0.0),(2.0,1.0),(3.0,0.0),(3.0,1.0)]

estos valores se han de sumar a la Casilla inferior izquierda de la pieza para saber donde se halla colocada en todo momento.
--}

colorPieza :: Pieza -> Color
colorPieza Verde = green
colorPieza Azul = blue
colorPieza Roja = red
colorPieza Naranja = orange
colorPieza Rosa = pink
colorPieza Amarilla = yellow

tablero :: [Casilla]
tablero = [(((-5.0)+1/2)+x,((-5.0)+1/2)+y)|x<-[0.0..9.0], y<-[0.0..9.0]]

{--
tablero

[(-4.5,-4.5),(-4.5,-3.5),(-4.5,-2.5),(-4.5,-1.5),(-4.5,-0.5),(-4.5,0.5),(-4.5,1.5),(-4.5,2.5),(-4.5,3.5),(-4.5,4.5),
(-3.5,-4.5),(-3.5,-3.5),(-3.5,-2.5),(-3.5,-1.5),(-3.5,-0.5),(-3.5,0.5),(-3.5,1.5),(-3.5,2.5),(-3.5,3.5),(-3.5,4.5),
(-2.5,-4.5),(-2.5,-3.5),(-2.5,-2.5),(-2.5,-1.5),(-2.5,-0.5),(-2.5,0.5),(-2.5,1.5),(-2.5,2.5),(-2.5,3.5),(-2.5,4.5),
(-1.5,-4.5),(-1.5,-3.5),(-1.5,-2.5),(-1.5,-1.5),(-1.5,-0.5),(-1.5,0.5),(-1.5,1.5),(-1.5,2.5),(-1.5,3.5),(-1.5,4.5),
(-0.5,-4.5),(-0.5,-3.5),(-0.5,-2.5),(-0.5,-1.5),(-0.5,-0.5),(-0.5,0.5),(-0.5,1.5),(-0.5,2.5),(-0.5,3.5),(-0.5,4.5),
(0.5,-4.5),(0.5,-3.5),(0.5,-2.5),(0.5,-1.5),(0.5,-0.5),(0.5,0.5),(0.5,1.5),(0.5,2.5),(0.5,3.5),(0.5,4.5),(1.5,-4.5),
(1.5,-3.5),(1.5,-2.5),(1.5,-1.5),(1.5,-0.5),(1.5,0.5),(1.5,1.5),(1.5,2.5),(1.5,3.5),(1.5,4.5),(2.5,-4.5),(2.5,-3.5),
(2.5,-2.5),(2.5,-1.5),(2.5,-0.5),(2.5,0.5),(2.5,1.5),(2.5,2.5),(2.5,3.5),(2.5,4.5),(3.5,-4.5),(3.5,-3.5),(3.5,-2.5),
(3.5,-1.5),(3.5,-0.5),(3.5,0.5),(3.5,1.5),(3.5,2.5),(3.5,3.5),(3.5,4.5),(4.5,-4.5),(4.5,-3.5),(4.5,-2.5),(4.5,-1.5),
(4.5,-0.5),(4.5,0.5),(4.5,1.5),(4.5,2.5),(4.5,3.5),(4.5,4.5)]

Casillas en el plano que conforman el tablero
--}

pintaTablero :: [Casilla] -> Picture
pintaTablero [] = blank
pintaTablero ((x,y):xs) = (translated x y (coloured black (solidRectangle 1 1))) & (pintaTablero xs)

calculaOpciones :: [Casilla] -> Pieza -> [Casilla]
calculaOpciones _ Ninguna = []
calculaOpciones recuadrosLibres piezaElegida = 
   [puntoPlano | puntoPlano <- recuadrosLibres, (calculaOpcionesAux puntoPlano (dimensionPieza piezaElegida) recuadrosLibres)]

{--
calculaOpciones tablero Verde

[(-4.5,-4.5),(-4.5,-3.5),(-4.5,-2.5),(-4.5,-1.5),(-4.5,-0.5),(-4.5,0.5),(-4.5,1.5),(-4.5,2.5),(-4.5,3.5),(-3.5,-4.5),
(-3.5,-3.5),(-3.5,-2.5),(-3.5,-1.5),(-3.5,-0.5),(-3.5,0.5),(-3.5,1.5),(-3.5,2.5),(-3.5,3.5),(-2.5,-4.5),(-2.5,-3.5),
(-2.5,-2.5),(-2.5,-1.5),(-2.5,-0.5),(-2.5,0.5),(-2.5,1.5),(-2.5,2.5),(-2.5,3.5),(-1.5,-4.5),(-1.5,-3.5),(-1.5,-2.5),
(-1.5,-1.5),(-1.5,-0.5),(-1.5,0.5),(-1.5,1.5),(-1.5,2.5),(-1.5,3.5),(-0.5,-4.5),(-0.5,-3.5),(-0.5,-2.5),(-0.5,-1.5),
(-0.5,-0.5),(-0.5,0.5),(-0.5,1.5),(-0.5,2.5),(-0.5,3.5),(0.5,-4.5),(0.5,-3.5),(0.5,-2.5),(0.5,-1.5),(0.5,-0.5),(0.5,0.5),
(0.5,1.5),(0.5,2.5),(0.5,3.5),(1.5,-4.5),(1.5,-3.5),(1.5,-2.5),(1.5,-1.5),(1.5,-0.5),(1.5,0.5),(1.5,1.5),(1.5,2.5),(1.5,3.5)]


dada una lista de posiciones vacias(inicialmente el tablero) y una pieza, devuelve la lista de posiciones en el tablero donde podemos
colocar la pieza utilizando como esquina inferior izquierda dicha posición
--}

calculaOpcionesAux :: Casilla -> [Casilla] -> [Casilla] -> Bool
calculaOpcionesAux (x, y) dimension recuadrosLibres = and[(x+k, y+z) `elem` recuadrosLibres| (k, z) <- dimension]

{--
calculaOpcionesAux (-4.5, -4.5) (dimensionPieza Verde) tablero

True

Comprueba que la suma de la dimensión de la pieza con el inicio de la pieza, todas esas casillas están libres
--}

pintaPieza :: Pieza -> [Casilla] -> [Pieza] -> Picture
pintaPieza _ [] piezasRestantes = blank
pintaPieza pieza recuadrosPieza piezasRestantes= 
   (ajustaEnumeracionPiezaColocada pieza (recuadrosPieza!!0) piezasRestantes) & 
   pictures[translated x y (coloured (colorPieza pieza) (solidRectangle 1 1))|(x, y)<-recuadrosPieza]
   
ajustaEnumeracionPiezaColocada :: Pieza -> Casilla -> [Pieza] -> Picture
ajustaEnumeracionPiezaColocada Verde (x, y) piezasRestantes
   |Verde `elem` piezasRestantes = (translated (x+1.5) (y+0.5) (coloured black (lettering "1")))
   |otherwise = (translated (x+1.5) (y+0.5) (coloured white (lettering "X"))) & (translated (x+1.5) (y+0.5) (coloured black (lettering "1")))
ajustaEnumeracionPiezaColocada Azul (x, y) piezasRestantes 
   |Azul `elem` piezasRestantes = (translated (x+1.5) (y+2.0) (coloured black (lettering "2")))
   |otherwise = (translated (x+1.5) (y+2.0) (coloured white (lettering "X"))) & (translated (x+1.5) (y+2.0) (coloured black (lettering "2")))
ajustaEnumeracionPiezaColocada Roja (x, y) piezasRestantes
   |Roja `elem` piezasRestantes = (translated (x+1.0) (y+3.5) (coloured black (lettering "3")))
   |otherwise = (translated (x+1.0) (y+3.5) (coloured white (lettering "X"))) & (translated (x+1.0) (y+3.5) (coloured black (lettering "2")))
ajustaEnumeracionPiezaColocada Naranja (x, y) piezasRestantes 
   |Naranja `elem` piezasRestantes = (translated (x+3.0) (y+1.0) (coloured black (lettering "4")))
   |otherwise = (translated (x+3.0) (y+1.0) (coloured white (lettering "X"))) & (translated (x+3.0) (y+1.0) (coloured black (lettering "4")))
ajustaEnumeracionPiezaColocada Rosa (x, y) piezasRestantes  
   |Rosa `elem` piezasRestantes = (translated (x+1.0) (y+3.0) (coloured black (lettering "5")))
   |otherwise = (translated (x+1.0) (y+3.0) (coloured white (lettering "X"))) & (translated (x+1.0) (y+3.0) (coloured black (lettering "5")))
ajustaEnumeracionPiezaColocada Amarilla (x, y) piezasRestantes
   |Amarilla `elem` piezasRestantes = (translated (x+1.0) (y+0.5) (coloured black (lettering "6")))
   |otherwise = (translated (x+1.0) (y+0.5) (coloured white (lettering "X"))) & (translated (x+1.0) (y+0.5) (coloured black (lettering "6")))

pintaAsteriscos :: [Casilla] -> Picture
pintaAsteriscos iniciosPieza = pictures[translated x (y-0.15) (coloured white (lettering "*"))|(x, y)<-iniciosPieza]

traduceEventoAPieza :: Event -> Pieza
traduceEventoAPieza (KeyPress "1") = Verde 
traduceEventoAPieza (KeyPress "2") = Azul 
traduceEventoAPieza (KeyPress "3") = Roja
traduceEventoAPieza (KeyPress "4") = Naranja 
traduceEventoAPieza (KeyPress "5") = Rosa
traduceEventoAPieza (KeyPress "6") = Amarilla

eventoEligeFicha :: [Event]
eventoEligeFicha = [KeyPress "1", 
                     KeyPress "2", 
                     KeyPress "3", 
                     KeyPress "4", 
                     KeyPress "5",
                     KeyPress "6"]
                     
pintaMundo :: Mundo -> Picture
pintaMundo (piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla) = 
   (pintaVictoria piezasRestantes) &
   (indicaPiezaSeleccionada piezaElegida) &
   (pintaAsteriscos (calculaOpciones recuadrosLibres piezaElegida)) &
   (pintaPieza Verde verde piezasRestantes) &
   (pintaPieza Azul azul piezasRestantes) &
   (pintaPieza Roja roja piezasRestantes) &
   (pintaPieza Naranja naranja piezasRestantes) &
   (pintaPieza Rosa rosa piezasRestantes) &
   (pintaPieza Amarilla amarilla piezasRestantes) & 
   (pintaPiezasDisponibles [(Verde, verde), (Azul, azul), (Roja, roja), (Naranja, naranja), (Rosa, rosa), (Amarilla, amarilla)]) &
   (pintaTablero (tablero)) -- &
   -- (coordinatePlane)

pintaVictoria :: [Pieza] -> Picture
pintaVictoria [] = scaled 2.0 2.0 (translated 0.0 0.0 (coloured black (lettering "Victoria")))
pintaVictoria _ = blank

pintaPiezasDisponibles :: [(Pieza, [Casilla])] -> Picture
pintaPiezasDisponibles ls = pictures[if length(coordPieza)==0 then (dibujaPiezaDisponible pieza) else blank|(pieza, coordPieza)<-ls]

dibujaPiezaDisponible :: Pieza -> Picture
dibujaPiezaDisponible Verde = 
   (translated (7.5) (-0.5) (coloured black (lettering "1"))) & 
   pictures[translated ((6.0)+k) ((-1.0)+z) (coloured green (solidRectangle 1 1))|(k, z)<-(dimensionPieza Verde)]
dibujaPiezaDisponible Azul = 
   (translated (7.5) (-7.0) (coloured black (lettering "2"))) & 
   pictures[translated ((6.0)+k) ((-9.0)+z) (coloured blue (solidRectangle 1 1))|(k, z)<-(dimensionPieza Azul)]
dibujaPiezaDisponible Roja = 
   (translated (-8.0) (-5.5) (coloured black (lettering "3"))) & 
   pictures[translated ((-9.0)+k) ((-9.0)+z) (coloured red (solidRectangle 1 1))|(k, z)<-(dimensionPieza Roja)]
dibujaPiezaDisponible Naranja = 
   (translated (-5.0) (7.0) (coloured black (lettering "4"))) & 
   pictures[translated ((-8.0)+k) ((6.0)+z) (coloured orange (solidRectangle 1 1))|(k, z)<-(dimensionPieza Naranja)]
dibujaPiezaDisponible Rosa = 
   (translated (7.0) (5.0) (coloured black (lettering "5"))) & 
   pictures[translated ((6.0)+k) ((2.0)+z) (coloured pink (solidRectangle 1 1))|(k, z)<-(dimensionPieza Rosa)]
dibujaPiezaDisponible Amarilla = 
   (translated (-8.0) (1.5) (coloured black (lettering "6"))) & 
   pictures[translated ((-9.0)+k) ((1.0)+z) (coloured yellow (solidRectangle 1 1))|(k, z)<-(dimensionPieza Amarilla)]

indicaPiezaSeleccionada :: Pieza -> Picture
indicaPiezaSeleccionada Ninguna =
   scaled 0.85 0.85 (translated (0.0) (-7.0) (coloured black (lettering "La pieza seleccionada es: ")) & translated (0.0) (-8.0) (coloured black (lettering "Ninguna")))
indicaPiezaSeleccionada Verde = 
   scaled 0.85 0.85 (translated (0.0) (-7.0) (coloured black (lettering "La pieza seleccionada es: ")) & translated (0.0) (-8.0) (coloured green (lettering "Verde (1)")))
indicaPiezaSeleccionada Azul =
   scaled 0.85 0.85 (translated (0.0) (-7.0) (coloured black (lettering "La pieza seleccionada es: ")) & translated (0.0) (-8.0) (coloured blue (lettering "Azul (2)")))
indicaPiezaSeleccionada Roja =
   scaled 0.85 0.85 (translated (0.0) (-7.0) (coloured black (lettering "La pieza seleccionada es: ")) & translated (0.0) (-8.0) (coloured red (lettering "Roja (3)")))
indicaPiezaSeleccionada Naranja =
   scaled 0.85 0.85 (translated (0.0) (-7.0) (coloured black (lettering "La pieza seleccionada es: ")) & translated (0.0) (-8.0) (coloured orange (lettering "Naranja (4)")))
indicaPiezaSeleccionada Rosa =
   scaled 0.85 0.85 (translated (0.0) (-7.0) (coloured black (lettering "La pieza seleccionada es: ")) & translated (0.0) (-8.0) (coloured pink (lettering "Rosa (5)")))
indicaPiezaSeleccionada Amarilla =
   scaled 0.85 0.85 (translated (0.0) (-7.0) (coloured black (lettering "La pieza seleccionada es: ")) & translated (0.0) (-8.0) (coloured yellow (lettering "Amarilla (6)")))


estadoInicial1 :: Mundo
estadoInicial1 = (piezas, Ninguna, tablero, [], [], [], [], [], [])

estadoInicial2 :: Mundo
estadoInicial2 = ([Azul, Roja, Naranja, Rosa, Amarilla], Azul,
                  quitaRecuadrosLibres tablero (construirPieza (-4.5,-4.5) (dimensionPieza Verde)),
                  construirPieza (-4.5,-4.5) (dimensionPieza Verde), [], [], [], [], [])

estadoInicial3 :: Mundo
estadoInicial3 = ([Azul, Roja, Naranja, Rosa, Amarilla], Ninguna,
                  quitaRecuadrosLibres (quitaRecuadrosLibres tablero (construirPieza (-4.5,-4.5) (dimensionPieza Verde))) (construirPieza (-4.5,-2.5) (dimensionPieza Azul)),
                  construirPieza (-4.5,-4.5) (dimensionPieza Verde), construirPieza (-4.5,-2.5) (dimensionPieza Azul), [], [], [], [])

resuelveEvento :: Event -> Mundo -> Mundo

resuelveEvento _ estado@([], _, _, _, _, _, _, _, _) = estado

resuelveEvento evento estado@(piezasRestantes, Ninguna, recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla) 
   |(not(evento `elem` eventoEligeFicha)) = estado
   |(not((traduceEventoAPieza evento) `elem` piezasRestantes)) = estado
   |(length(verde) /= 0) && ((traduceEvento) == Verde) = 
      (piezasRestantes, Verde, recuadrosLibres++verde, verde, azul, roja, naranja, rosa, amarilla)
   |(length(azul) /= 0) && ((traduceEvento) == Azul) = 
      (piezasRestantes, Azul, recuadrosLibres++azul, verde, azul, roja, naranja, rosa, amarilla)
   |(length(roja) /= 0) && ((traduceEvento) == Roja) = 
      (piezasRestantes, Roja, recuadrosLibres++roja, verde, azul, roja, naranja, rosa, amarilla)
   |(length(naranja) /= 0) && ((traduceEvento) == Naranja) = 
      (piezasRestantes, Naranja, recuadrosLibres++naranja, verde, azul, roja, naranja, rosa, amarilla)
   |(length(rosa) /= 0) && ((traduceEvento) == Rosa) = 
      (piezasRestantes, Rosa, recuadrosLibres++rosa, verde, azul, roja, naranja, rosa, amarilla)
   |(length(amarilla) /= 0) && ((traduceEvento) == Amarilla) = 
      (piezasRestantes, Amarilla, recuadrosLibres++amarilla, verde, azul, roja, naranja, rosa, amarilla)
   |otherwise = (piezasRestantes, (traduceEvento), recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla)
   where traduceEvento = (traduceEventoAPieza evento)
   
resuelveEvento (PointerPress (x, y)) estado@(piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla)
   |not(tC `elem` (calculaOpciones recuadrosLibres piezaElegida)) = estado
   |piezaElegida == Verde = (piezasRestantes, piezaElegida, recuadrosLibres, cP, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Azul = (piezasRestantes, piezaElegida, recuadrosLibres, verde, cP, roja, naranja, rosa, amarilla)
   |piezaElegida == Roja = (piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, cP, naranja, rosa, amarilla)
   |piezaElegida == Naranja = (piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, cP, rosa, amarilla)
   |piezaElegida == Rosa = (piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, cP, amarilla)
   |piezaElegida == Amarilla = (piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, rosa, cP)
   where tC = (truncarCoordenadas (x, y))
         cP = construirPieza tC (dimensionPieza piezaElegida)
         
resuelveEvento (KeyPress "Ctrl") estado@(piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Verde && length(verde) == 0 = estado
   |piezaElegida == Azul && length(azul) == 0 = estado
   |piezaElegida == Roja && length(roja) == 0 = estado
   |piezaElegida == Naranja && length(naranja) == 0 = estado
   |piezaElegida == Rosa && length(rosa) == 0 = estado
   |piezaElegida == Amarilla && length(amarilla) == 0 = estado 
   
resuelveEvento (KeyPress "Ctrl") estado@(piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Verde = (piezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres verde), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Azul = (piezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres azul), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Roja = (piezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres roja), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Naranja = (piezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres naranja), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Rosa = (piezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres rosa), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Amarilla = (piezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres amarilla), verde, azul, roja, naranja, rosa, amarilla)
   
resuelveEvento (KeyPress "Enter") estado@(piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Verde && length(verde) == 0 = estado
   |piezaElegida == Azul && length(azul) == 0 = estado
   |piezaElegida == Roja && length(roja) == 0 = estado
   |piezaElegida == Naranja && length(naranja) == 0 = estado
   |piezaElegida == Rosa && length(rosa) == 0 = estado
   |piezaElegida == Amarilla && length(amarilla) == 0 = estado 
         
resuelveEvento (KeyPress "Enter") estado@(piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Verde = (nuevasPiezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres verde), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Azul = (nuevasPiezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres azul), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Roja = (nuevasPiezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres roja), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Naranja = (nuevasPiezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres naranja), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Rosa = (nuevasPiezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres rosa), verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Amarilla = (nuevasPiezasRestantes, Ninguna, (quitaRecuadrosLibres recuadrosLibres amarilla), verde, azul, roja, naranja, rosa, amarilla)
   where nuevasPiezasRestantes = (quitaPieza piezasRestantes piezaElegida)
   
resuelveEvento (KeyPress "Esc") estado@(piezasRestantes, piezaElegida, recuadrosLibres, verde, azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Verde = (piezasRestantes, Ninguna, recuadrosLibres, [], azul, roja, naranja, rosa, amarilla)
   |piezaElegida == Azul = (piezasRestantes, Ninguna, recuadrosLibres, verde, [], roja, naranja, rosa, amarilla)
   |piezaElegida == Roja = (piezasRestantes, Ninguna, recuadrosLibres, verde, azul, [], naranja, rosa, amarilla)
   |piezaElegida == Naranja = (piezasRestantes, Ninguna, recuadrosLibres, verde, azul, roja, [], rosa, amarilla)
   |piezaElegida == Rosa = (piezasRestantes, Ninguna, recuadrosLibres, verde, azul, roja, naranja, [], amarilla)
   |piezaElegida == Amarilla = (piezasRestantes, Ninguna, recuadrosLibres, verde, azul, roja, naranja, rosa, [])
   
resuelveEvento _ estado = estado

truncarCoordenadas :: Casilla -> Casilla
truncarCoordenadas (x, y) = ((fromIntegral(floor x))+1/2, (fromIntegral(floor y))+1/2)

{--
truncarCoordenadas (-4.28,-4.31)

(-4.5,-4.5)


al usar el floor, se me queda en (-5.0, -5.0) y si le sumo el 1/2 a ambas, me queda (-4.5,-4.5)
--}


construirPieza :: Casilla -> [Casilla] -> [Casilla]
construirPieza (x, y) dimension = [(x+k, y+z)|(k, z)<-dimension]

{--
construirPieza (-4.5,-4.5) (dimensionPieza Verde)

[(-4.5,-4.5),(-4.5,-3.5),(-3.5,-4.5),(-3.5,-3.5),(-2.5,-4.5),(-2.5,-3.5),(-1.5,-4.5),(-1.5,-3.5)]

pieza construida con inicio en esquina inferior izquierda de coordenadas (-4.5,-4.5)
--}

quitaPieza :: [Pieza] -> Pieza -> [Pieza]
quitaPieza [x] pieza = []
quitaPieza (x:xs) pieza
   |pieza == x = xs
   |otherwise = x:(quitaPieza xs pieza)

{--
quitaPieza [Verde, Azul] Verde


[Azul]

quita la pieza verde de la lista
--}
   
quitaRecuadrosLibres :: [Casilla] -> [Casilla] -> [Casilla]
quitaRecuadrosLibres recuadrosLibres [] = recuadrosLibres
quitaRecuadrosLibres recuadrosLibres (x:xs) = 
   (quitaRecuadrosLibres (quitaRecuadrosLibresAux recuadrosLibres x) xs)

{--
quitaRecuadrosLibres tablero (construirPieza (-4.5,-4.5) (dimensionPieza Verde))

[(-4.5,-2.5),(-4.5,-1.5),(-4.5,-0.5),(-4.5,0.5),(-4.5,1.5),(-4.5,2.5),(-4.5,3.5),(-4.5,4.5),(-3.5,-2.5),(-3.5,-1.5),
(-3.5,-0.5),(-3.5,0.5),(-3.5,1.5),(-3.5,2.5),(-3.5,3.5),(-3.5,4.5),(-2.5,-2.5),(-2.5,-1.5),(-2.5,-0.5),(-2.5,0.5),
(-2.5,1.5),(-2.5,2.5),(-2.5,3.5),(-2.5,4.5),(-1.5,-2.5),(-1.5,-1.5),(-1.5,-0.5),(-1.5,0.5),(-1.5,1.5),(-1.5,2.5),
(-1.5,3.5),(-1.5,4.5),(-0.5,-4.5),(-0.5,-3.5),(-0.5,-2.5),(-0.5,-1.5),(-0.5,-0.5),(-0.5,0.5),(-0.5,1.5),(-0.5,2.5),
(-0.5,3.5),(-0.5,4.5),(0.5,-4.5),(0.5,-3.5),(0.5,-2.5),(0.5,-1.5),(0.5,-0.5),(0.5,0.5),(0.5,1.5),(0.5,2.5),(0.5,3.5),
(0.5,4.5),(1.5,-4.5),(1.5,-3.5),(1.5,-2.5),(1.5,-1.5),(1.5,-0.5),(1.5,0.5),(1.5,1.5),(1.5,2.5),(1.5,3.5),(1.5,4.5),
(2.5,-4.5),(2.5,-3.5),(2.5,-2.5),(2.5,-1.5),(2.5,-0.5),(2.5,0.5),(2.5,1.5),(2.5,2.5),(2.5,3.5),(2.5,4.5),(3.5,-4.5),
(3.5,-3.5),(3.5,-2.5),(3.5,-1.5),(3.5,-0.5),(3.5,0.5),(3.5,1.5),(3.5,2.5),(3.5,3.5),(3.5,4.5),(4.5,-4.5),(4.5,-3.5),
(4.5,-2.5),(4.5,-1.5),(4.5,-0.5),(4.5,0.5),(4.5,1.5),(4.5,2.5),(4.5,3.5),(4.5,4.5)]

teniendo el tablero completamente vacio, si coloco la pieza verde comenzando en la posición (-4.5,-4.5),
devuelve una lista actualizada con todas las casillas vacias(100-8=92)
--}

quitaRecuadrosLibresAux :: [Casilla] -> Casilla -> [Casilla]
quitaRecuadrosLibresAux [x] recuadroLibre = []
quitaRecuadrosLibresAux (x:xs) recuadroLibre 
   |x == recuadroLibre = xs
   |otherwise = x:(quitaRecuadrosLibresAux xs recuadroLibre)

{--
quitaRecuadrosLibresAux [(-4.5,-4.5),(-4.5,-3.5)] (-4.5,-4.5)

[(-4.5,-3.5)]
--}
   
juego :: IO()
juego = activityOf (estadoInicial1) resuelveEvento pintaMundo
main :: IO()
main = juego