import Text.Show.Functions()
import Data.List (genericLength)


--------------------------------------------
alterarDinero :: (Int -> Int -> Int ) -> Int -> Persona -> Persona
alterarDinero operador cantidad persona = persona {cantidadDeDinero = (operador) (cantidadDeDinero persona) cantidad }
------------------------------------------
agregarAccion :: Persona -> Accion -> Persona
agregarAccion persona accion = persona {acciones = accion:acciones persona}

comprarPropiedad :: Propiedad -> Persona -> Persona
comprarPropiedad propiedad persona = alterarDinero (-) (precio propiedad) persona {propiedadesCompradas = propiedad:propiedadesCompradas persona}

--PUNTO 2 

pasarPorElBanco :: Accion
pasarPorElBanco persona = alterarDinero (+) 40 persona {tacticaDeJuego = "Comprador compulsivo"}
------
enojarse :: Accion
enojarse persona = alterarDinero (+) 50 $ agregarAccion persona gritar
------
gritar :: Accion
gritar persona = persona {nombre = "AHHH "++ nombre persona}
-----
tieneLaTactica :: Persona -> Bool
tieneLaTactica persona = tacticaDeJuego persona == "Oferente singular" || tacticaDeJuego persona == "Accionista" 


                         
subastar :: Propiedad -> Accion
subastar propiedad persona | tieneLaTactica persona = comprarPropiedad propiedad persona
                           | otherwise = persona         
---------------------------------
esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata propiedad = (>) 150 (precio propiedad)

cantPropiedadesSegun :: Persona -> (Propiedad -> Bool) -> Int
cantPropiedadesSegun persona condicion = length $ filter condicion (propiedadesCompradas persona) --Si quiero las propiedades --                                                          baratas le paso como condicion esPropiedadBarata, sino not.esPropiedadBarata

------------------------------
valorDePropiedad :: Propiedad -> Int
valorDePropiedad propiedad
  | esPropiedadBarata propiedad = 10
  | otherwise = 20

gananciaPorPropiedad :: [Propiedad] -> [Int]
gananciaPorPropiedad = map valorDePropiedad

gananciaTotal :: Persona -> Int
gananciaTotal persona = sum $ gananciaPorPropiedad $ propiedadesCompradas persona

cobrarAlquileres :: Accion
cobrarAlquileres persona = alterarDinero (+) (gananciaTotal persona) persona

-------------------------------
esAccionista :: Persona -> Bool
esAccionista persona = tacticaDeJuego persona == "Accionista"

pagarAAccionistas :: Accion
pagarAAccionistas persona | esAccionista persona  = alterarDinero (+) 200 persona
                          | otherwise = alterarDinero (-) 100 persona
                          
--------------------------                                      
hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad persona | cantidadDeDinero persona < precio propiedad = hacerBerrinchePor propiedad $ alterarDinero (+) 10 $ gritar persona 
                                    | otherwise = comprarPropiedad propiedad persona
-----------
ultimaRonda :: Persona -> Accion
ultimaRonda persona = foldl (.) (head $ acciones persona) (tail $ acciones persona)

dineroFinal :: Persona -> Int
dineroFinal persona1  =  cantidadDeDinero $ ultimaRonda persona1 persona1

juegoFinal :: Persona -> Persona -> Persona
juegoFinal persona1 persona2 | dineroFinal persona1 > dineroFinal persona2 = persona1   
                             | otherwise = persona2
-------------------
--PUNTO 3 


type Accion = (Persona -> Persona) 


data Persona = UnaPersona {
    nombre               :: String,
    cantidadDeDinero     :: Int,
    tacticaDeJuego       :: String,
    propiedadesCompradas :: [Propiedad],
    acciones             :: [Accion]
} deriving Show


data Propiedad = UnaPropiedad {
    nombrePropiedad :: String,
    precio :: Int
} deriving Show

--------------------------------------------------

--PUNTO 1

carolina = UnaPersona {
    nombre = "Carolina",
    cantidadDeDinero = 500,
    tacticaDeJuego = "Accionista",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco,pagarAAccionistas]
}

manuel = UnaPersona {
    nombre = "Manuel",
    cantidadDeDinero = 500,
    tacticaDeJuego = "Oferente singular",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco,enojarse]
}

