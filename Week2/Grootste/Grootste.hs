grootste [] y = y
grootste (x:xs) y
	| x >= y = grootste xs x
	| otherwise = grootste xs x

main = print(grootste [1,2,3,4,5])