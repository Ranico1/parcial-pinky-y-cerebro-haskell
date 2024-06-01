

data Animal = Animal {
coeficienteIntelectual :: Int,
especie :: Especie,
capacidades :: [Capacidad]
}

type Especie = String
type Capacidad = String 

--PUNTO 2
inteligenciaSuperior :: Int -> Animal -> Animal
inteligenciaSuperior n unAnimal = unAnimal {coeficienteIntelectual = coeficienteIntelectual unAnimal * n} 

pinkificar :: Animal -> Animal
pinkificar unAnimal  = unAnimal {capacidades = []}

superPoderes :: Animal -> Animal
superPoderes unAnimal 
    | especie unAnimal == "Elefante" = mapCapacidades ("no tenerle miedo a los ratones" :) unAnimal
    | especie unAnimal == "raton" && coeficienteIntelectual unAnimal > 100 = mapCapacidades ("hablar" : ) unAnimal
    | otherwise = unAnimal 


mapCapacidades :: ([Capacidad] -> [Capacidad]) -> Animal -> Animal 
mapCapacidades f unAnimal = unAnimal {capacidades = f $ capacidades unAnimal}

--PUNTO 3
antropomorfico :: Animal -> Bool
antropomorfico unAnimal = elem "hablar" (capacidades unAnimal) && coeficienteIntelectual unAnimal > 60


noTanCuerdo :: Animal -> Bool
noTanCuerdo = (>2).length.filter (pinkiesko). capacidades

pinkiesko :: Capacidad -> Bool 
pinkiesko unaCapacidad = take 5 unaCapacidad == "hacer" &&  pinkieska (drop 6 unaCapacidad)

pinkieska :: String -> Bool 
pinkieska palabra = length palabra <= 4 && any hayVocal palabra 

hayVocal :: Char -> Bool 
hayVocal letra = elem letra "aeiouAEIOU" 

-- PUNTO 4 
data Experimento = Experimento {
    transformaciones :: [Animal -> Animal],
    criterio :: Animal -> Bool
}

type Criterio = Animal -> Bool 
type Transformaciones = [Animal -> Animal]


experimentoExitoso :: Criterio -> Experimento -> Animal -> Bool 
experimentoExitoso unCriterio unExperimento = unCriterio . aplicarTransformaciones (transformaciones unExperimento)  

aplicarTransformaciones :: Transformaciones -> Animal -> Animal 
aplicarTransformaciones transformaciones unAnimal = foldl (\x f -> f x ) unAnimal transformaciones 

--Modelar raton
raton = Animal {
coeficienteIntelectual = 17,
especie = "Raton",
capacidades = ["destruenglonir el mundo","hacer planes desalmados"]
}

experimentoRaton = Experimento {
transformaciones = [pinkificar,inteligenciaSuperior 10, superPoderes],
criterio = antropomorfico    
}


--PUNTO 5 

--informe :: [Animal] -> [Capacidad] -> [Transformaciones] -> [Int]
--informe animales capacidades transformaciones = map (aplicarTransformaciones transformaciones) animales


-- comparar :: [Capacidad] -> Capacidad -> Bool
-- comparar [] = False
-- comparar x capacidad = capacidad == x
-- comparar (x:xs) = capacidad == x || comparar (xs) capacidad 