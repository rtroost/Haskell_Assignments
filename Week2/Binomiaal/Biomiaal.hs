fac :: Integral a => a -> a
fac n
	| n == 0 = 1
	| otherwise = n * fac(n - 1)

binocoef :: Integral a => a -> a -> a
binocoef n k = fac n `div` (fac k * fac(n-k))

main = 	print (binocoef 5 3)