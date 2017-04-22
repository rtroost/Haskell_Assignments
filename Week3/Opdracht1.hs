data Kleur = Rood | Blauw | Geel deriving (Show)
data Geofig = Vierkant Float Kleur | Rechthoek Float Float Kleur | Driehoek Float Kleur | Cirkel Float Kleur deriving (Show)


oppervlakte :: Geofig -> Float
oppervlakte (Vierkant lb k) = 2 * lb
oppervlakte (Rechthoek h b k) = h * b
oppervlakte (Driehoek z k) = (z^2) * (sqrt(3)/4)
oppervlakte (Cirkel r k) = pi * r ^ 2

omtrek :: Geofig -> Float
omtrek (Vierkant lb k) = lb * 4
omtrek (Rechthoek h b k) = (h  * 2) + (b * 2)
omtrek (Driehoek z k) = z * 3
omtrek (Cirkel r k) = pi * (r * 2)

alleenvierkanten :: [Geofig] -> [Vierkant]
alleenvierkanten [] = []
alleenvierkanten (x:xs) = x : alleenvierkanten xs



maakeenvierkant = Vierkant 5.0 Rood
maakeenrechthoek = Rechthoek 5.0 10.0 Blauw
maakeendriehoek = Driehoek 9.0 Geel
maakeencirkel = Cirkel 20.0 Rood

main = do
	print (maakeenvierkant)
	print (maakeenrechthoek)
	print (maakeendriehoek)
	print (maakeencirkel)
	print ("Oppervlakte vierkant: " ++ show (oppervlakte(maakeenvierkant)))
	print ("Oppervlakte rechthoek: " ++ show (oppervlakte(maakeenrechthoek)))
	print ("Oppervlakte driehoek: " ++ show (oppervlakte(maakeendriehoek)))
	print ("Oppervlakte cirkel: " ++ show (oppervlakte(maakeencirkel)))
	print ("Omtrek vierkant: " ++ show (omtrek(maakeenvierkant)))
	print ("Omtrek rechthoek: " ++ show (omtrek(maakeenrechthoek)))
	print ("Omtrek driehoek: " ++ show (omtrek(maakeendriehoek)))
	print ("Omtrek cirkel: " ++ show (omtrek(maakeencirkel)))