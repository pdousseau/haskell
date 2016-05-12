module Projeto1 (volumeEsfera, areaEsfera, volumeCilindro, areaCilindro,
					areaLateralCilindro,  volumeCone, areaCone, 
					areaLateralCone, volumeTroncoCone, areaTroncoCone, 
					areaLateralTronco, volumeEsferoideOblato, 
					areaEsferoideOblato, volumeEsferoideProlato,
					areaEsferoideProlato, excentricidade) where

type Altura = Float
type Raio = Float
type Area = Float
type Volume = Float

volumeEsfera :: Raio -> Volume
volumeEsfera r = 4*pi*r*r*r / 3

areaEsfera :: Raio -> Area
areaEsfera r = 4*pi*r*r

volumeCilindro :: Raio -> Altura -> Volume
volumeCilindro r a = pi*r*r*a

areaCilindro :: Raio -> Altura -> Area
areaCilindro r a = (areaLateralCilindro r a) + 2*pi*r*r

areaLateralCilindro :: Raio -> Altura -> Area
areaLateralCilindro r a = 2*pi*r*a

volumeCone :: Raio -> Altura -> Volume
volumeCone r a = pi*r*r*a/3

areaCone :: Raio -> Altura -> Area
areaCone r a = pi*r*((sqrt (r*r + a*a)) + r)

areaLateralCone :: Raio -> Altura -> Area
areaLateralCone r a = pi*r*(sqrt (r*r + a*a))

volumeTroncoCone :: Raio -> Raio -> Altura -> Volume
volumeTroncoCone r1 r2 a = pi*a*(r1*r1 + r2*r2 + r1*r2)/3

areaTroncoCone :: Raio -> Raio -> Altura -> Area
areaTroncoCone r1 r2 a = pi*r1*r1 + pi*r2*r2 + (areaLateralTronco r1 r2 a)

areaLateralTronco :: Raio -> Raio -> Altura -> Area
areaLateralTronco r1 r2 a = pi*(r1 + r2)*(sqrt (a*a + r1*r1 - r2*r2))

volumeEsferoideOblato :: Raio -> Raio ->Volume
volumeEsferoideOblato a b = 4*pi*a*a*b/3

areaEsferoideOblato :: Raio -> Raio ->Area
areaEsferoideOblato a b = 2*pi*a*a + b*b*(log ((1 + (excentricidade a b))/
							(1 - (excentricidade a b))))/(excentricidade a b)

excentricidade :: Raio -> Raio -> Float
excentricidade a b = (sqrt (a*a - b*b)) /a

volumeEsferoideProlato :: Raio -> Raio -> Area
volumeEsferoideProlato a b = 4*pi*a*b*b/3

areaEsferoideProlato :: Raio -> Raio -> Volume
areaEsferoideProlato a b = 2*pi*b*b + 2*pi*(a*b/(excentricidade a b))
							*(asin (excentricidade a b))
