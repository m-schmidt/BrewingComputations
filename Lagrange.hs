module Lagrange (interpolate) where

import Data.List


-- |Compute interpolated value for point `x` from data points `xs` mapping to values `ys`.
interpolate :: Fractional a => a -> [a] -> [a] -> a
interpolate x xs ys = lagrange x dataTriples
  where
    dataTriples = zip3 ys xs $ sublists xs


-- |Lagrange interpolation polynomial.
lagrange :: Fractional a => a -> [(a, a, [a])] -> a
lagrange x = sum . map yL
  where
    yL (yj, xj, xs) = yj * basePolynomial x xj xs


-- |Lagrange base polynomial `L(x)`
basePolynomial :: Fractional a => a -> a -> [a] -> a
basePolynomial x xj xis = product $ map (\xi -> (x - xi) / (xj - xi)) xis


-- |Generate all sublists of `xs` where the nth list of the result contains all but the nth element of `xs`.
-- Example:
--
-- > sublists [1..4] == [[2,3,4], [1,3,4], [1,2,4], [1,2,3]]
sublists :: [a] -> [[a]]
sublists [] = []
sublists xs = reverse $ combinations (length xs - 1) xs
  where
    combinations 0 _  = [[]]
    combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs' ]
