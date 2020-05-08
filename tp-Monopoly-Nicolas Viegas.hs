import Text.Show.Functions()
import Data.List (genericLength)

-------------------------------------------------------
--PUNTO 2 


pasarPorElBanco :: Accion
pasarPorElBanco persona = UnaPersona (nombre persona) (cantidadDeDinero persona+40) "Comprador compulsivo" (propiedadesCompradas persona) (acciones persona)

------
enojarse :: Accion
enojarse persona = UnaPersona (nombre persona) (cantidadDeDinero persona+50) (tacticaDeJuego persona) (propiedadesCompradas persona) ([gritar]++(acciones persona))

------
gritar :: Accion
gritar persona = UnaPersona("AHHHH" ++ nombre persona) (cantidadDeDinero persona) (tacticaDeJuego persona) (propiedadesCompradas persona) (acciones persona)

-----
tieneLaTactica :: Persona -> Bool
tieneLaTactica persona = tacticaDeJuego persona == "Oferente singular" || tacticaDeJuego persona == "Accionista" 

subastar :: Persona -> Propiedad -> Persona
subastar persona propiedad | (tieneLaTactica persona == True) = UnaPersona (nombre persona) (cantidadDeDinero persona - precio propiedad)                                                                                                          (tacticaDeJuego persona) (propiedadesCompradas persona ++ [propiedad]) (acciones persona)
                           | otherwise = persona

---------------------------------
esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata propiedad = (>) 150 (precio propiedad)

cantPropiedadesSegun :: Persona -> (Propiedad -> Bool) -> Int
cantPropiedadesSegun persona condicion = length.filter (==True) $ map condicion (propiedadesCompradas persona) --Si quiero las propiedades --                                                          baratas le paso como condicion esPropiedadBarata, sino not.esPropiedadBarata

plataDelAlquiler :: Persona -> Int 
plataDelAlquiler persona = ((cantPropiedadesSegun persona esPropiedadBarata)*10) + ((cantPropiedadesSegun persona (not.esPropiedadBarata)*20))

cobrarAlquiler :: Accion 
cobrarAlquiler persona = UnaPersona (nombre persona) (cantidadDeDinero persona + plataDelAlquiler persona) (tacticaDeJuego persona) (propiedadesCompradas persona) (acciones persona)

----------------------------
esAccionista :: Persona -> Bool
esAccionista persona = tacticaDeJuego persona == "Accionista"

pagarAAccionistas :: Accion
pagarAAccionistas persona | (esAccionista persona == True) = UnaPersona (nombre persona) (cantidadDeDinero persona + 200) (tacticaDeJuego persona) (propiedadesCompradas persona) (acciones persona)
                          | otherwise = UnaPersona (nombre persona) (cantidadDeDinero persona - 100) (tacticaDeJuego persona) (propiedadesCompradas persona) (acciones persona)

--------------------------------------------------------
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



