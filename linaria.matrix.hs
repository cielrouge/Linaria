{-# LANGUAGE NamedFieldPuns #-}

module Linaria.Matrix where

data Matrix a = Matrix { nrows :: Int	
						, ncols :: Int
						, mElems :: [a]
   						} deriving Show

takeRow :: Matrix a -> Int -> [a]
takeRow (Matrix _ ncols mElems) ix = let start = (ix - 1) * ncols; end = ix * ncols - 1 in
						  					[start .. end] >>= \x -> 
												return $ mElems !! x 

takeCol :: Matrix a -> Int -> [a]
takeCol (Matrix nrows ncols mElems) ix = let ixs = [x | x <- map (\a -> a * ncols + ix - 1) [0 .. nrows - 1]] in
											ixs >>= \x ->
												return $ mElems !! x

takeElem :: Matrix a -> Int -> Int -> a  
takeElem m i j = (takeRow m i) !! (j - 1)

matrixAdd :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixAdd m1 m2 | (nrows m1) /= (nrows m1) || ((ncols m1) /= (ncols m2)) = Matrix 0 0 []
				| otherwise = let list = [1 .. nrows m1] >>= \i -> 
				  							zipWith (+) (takeRow m1 i) (takeRow m2 i)
				  			  in Matrix (nrows m1) (ncols m1) list   

scalarMult :: (Num a) => a -> Matrix a -> Matrix a
scalarMult k m@(Matrix { mElems = xs }) = let xs' = map ((*) k) xs in 
											m{ mElems = xs' }

matrixMult :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixMult m1 m2 | (ncols m1) /= (nrows m2) = Matrix 0 0 [] 
				 | otherwise = let list = [1 .. nrows m1] >>= \i ->
				 							[1 .. ncols m2] >>= \j ->
				 								return $ sum $ zipWith (*) (takeRow m1 i) (takeCol m2 j)
				 				in Matrix (nrows m1) (ncols m2) list

transpose :: Matrix a -> Matrix a
transpose m = Matrix (ncols m) (nrows m) list where 
					list = [1 .. ncols m] >>= \i -> 
								takeCol m i

-- :l linaria.matrix.hs
