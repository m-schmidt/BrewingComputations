module DeClerck
  ( extractForGravity
  , gravityForExtract
  ) where

import Common


-- |DeClerck formula: compute extract (°P) from specific gravity (SG).
extractForGravity :: Gravity -> Extract
extractForGravity = extractForGravity_default

-- |Inverse of DeClerck formula for relevant input range: compute specific gravity (SG) from extract (°P).
gravityForExtract :: Extract -> Gravity
gravityForExtract = gravityForExtract_default


-- |Default implementation: well known quadrativ formula and its inverse
extractForGravity_default :: Gravity -> Extract
extractForGravity_default sg = -463.37 + 668.72 * sg - 205.347 * sg**2

gravityForExtract_default :: Extract -> Gravity
gravityForExtract_default e = (334360 - sqrt (16644970210 - 205347000 * e)) / 205347


-- |Alternate implementation: cubic formula and its inverse
extractForGravity_cubic :: Gravity -> Extract
extractForGravity_cubic sg = -668.962 + 1262.45 * sg - 776.43 * sg**2 + 182.94 * sg**3

gravityForExtract_cubic :: Extract -> Gravity
gravityForExtract_cubic e = 1.41473 - 3.1967e-5 * x**(1/3) + 9.34868e3 / x**(1/3)
  where
    x = 3049 * sqrt (7.53008e14 * e**2 - 1.2209e17 * e + 7.63933e18) - 8.36676e10 * e + 6.7828e12
