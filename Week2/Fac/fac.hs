fac :: Integer -> Integer
fac n = if n == 0 then 1 else n * fac (n-1)

fac2 :: Integer -> Integer
fac2 n
	| n==0 = 1
	| otherwise = n * fac2 (n-1)

fac3 :: Integer -> Integer
fac3 n = foldr (*) 1 [1..n]

main = do
	print(fac 3)
	print(fac2 3)
	print(fac3 3)