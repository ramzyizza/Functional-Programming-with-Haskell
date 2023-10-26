--Write a function to pair each element of a list with its index

func :: [a] -> [(a,Int)]
func x = zip x [0 .. length x]
