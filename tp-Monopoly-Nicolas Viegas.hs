import Text.Show.Functions()
import Data.List (genericLength)

-------------------------------------------------------
--PUNTO 2 


pasarPorElBanco :: Accion
pasarPorElBanco persona = persona {cantidadDeDinero = cantidadDeDinero persona+40, tacticaDeJuego = "Comprador compulsivo"}
------
enojarse :: Accion
enojarse persona = persona {cantidadDeDinero = cantidadDeDinero persona+50, acciones = [gritar]++acciones persona}
------
gritar :: Accion
gritar persona = persona {nombre = "AHHH"++ nombre persona}
-----
tieneLaTactica :: Persona -> Bool
tieneLaTactica persona = tacticaDeJuego persona == "Oferente singular" || tacticaDeJuego persona == "Accionista" 

                         
subastar :: Propiedad -> Accion
subastar propiedad persona | (tieneLaTactica persona == True) = (persona {cantidadDeDinero = cantidadDeDinero persona - precio propiedad, propiedadesCompradas =  propiedadesCompradas persona ++ [propiedad]})
                           | otherwise = persona                        
---------------------------------
esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata propiedad = (>) 150 (precio propiedad)

cantPropiedadesSegun :: Persona -> (Propiedad -> Bool) -> Int
cantPropiedadesSegun persona condicion = length.filter (==True) $ map condicion (propiedadesCompradas persona) --Si quiero las propiedades --                                                          baratas le paso como condicion esPropiedadBarata, sino not.esPropiedadBarata

plataDelAlquiler :: Persona -> Int 
plataDelAlquiler persona = ((cantPropiedadesSegun persona esPropiedadBarata)*10) + ((cantPropiedadesSegun persona (not.esPropiedadBarata)*20))

cobrarAlquiler :: Accion 
cobrarAlquiler persona = persona {cantidadDeDinero = cantidadDeDinero persona + plataDelAlquiler persona }
----------------------------
esAccionista :: Persona -> Bool
esAccionista persona = tacticaDeJuego persona == "Accionista"

pagarAAccionistas :: Accion
pagarAAccionistas persona | (esAccionista persona == True) = (persona {cantidadDeDinero = cantidadDeDinero persona + 200})
                          | otherwise = persona{cantidadDeDinero = cantidadDeDinero persona - 100}
                          
                    
---------------------------
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

---------------------------
