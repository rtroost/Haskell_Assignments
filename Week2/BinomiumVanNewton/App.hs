summation :: Integer -> Integer -> Integer
summation x y = summation' x y 0

summation' :: Integer -> Integer -> Integer -> Integer
summation' x y sum
	| (y<x) = sum
	| otherwise = summation' x (y-1) (sum+y)



main = 	print ("lol")