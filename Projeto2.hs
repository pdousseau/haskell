module Projeto2 (quaseDois, quaseE, somaImpar, somaPar, 
				 somaQuadradoImpar, somaQuadradoInt) where

fatorial :: Float -> Float
fatorial 0 = 1
fatorial n = product [1..n]

quaseDois :: Float -> Float
quaseDois n = sum (map (\x->(2/(x^2+x))) [1.0..n])

quaseE :: Float -> Float
quaseE n = sum (map (\x->(1/(fatorial x))) [0.0..n])

somaImpar :: Int -> Int
somaImpar n = last (map (\x->(x*x)) [1..n])

somaPar :: Int -> Int
somaPar n = last (map (\x->(x + x*x)) [1..n])

somaQuadradoImpar :: Int -> Int
somaQuadradoImpar n = sum (map (\x->((2*x-1)^2)) [1..n])

somaQuadradoInt :: Int -> Int
somaQuadradoInt n = sum (map (\x->(x*x)) [1..n])


