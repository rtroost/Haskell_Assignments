myzip :: [a]->[b]->[(a,b)]
myzip [] ys = []
myzip xs [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

main = myzip [1,2,3]