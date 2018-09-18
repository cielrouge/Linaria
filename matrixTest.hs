testMatrix :: Matrix Int
testMatrix = Matrix 2 3 [1,2,3,4,5,6]

takeRow testMatrix 2

takeCol testMatrix 3

testMatrix2 :: Matrix Int
testMatrix2 = Matrix 4 5 [1..20]

takeElem testMatrix2 3 4

scalarMult 4 testMatrix2

testMatrix3 :: Matrix Int
testMatrix3 = Matrix 2 3 [1,2,3,2,3,1]

testMatrix4 :: Matrix Int
testMatrix4 = Matrix 3 3 [5,1,2,6,2,1,7,1,1]

matrixMult testMatrix3 testMatrix4 
-- Matrix {nrows = 2, ncols = 3, mElems = [38,8,7,35,9,8]}

matrixAdd testMatrix testMatrix3
-- Matrix {nrows = 2, ncols = 3, mElems = [2,4,6,6,8,7]}

transpose testMatrix4 
-- Matrix {nrows = 3, ncols = 3, mElems = [5,6,7,1,2,1,2,1,1]}