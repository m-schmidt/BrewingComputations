module Aerometer where

import Common

-- |Compute actual extract from original extract (°P) and apparent extract (°P).
actualExtract :: Extract -> Extract -> Extract
actualExtract oe ae = 0.1808 * oe + 0.8192 * ae

-- |Compute apparent/actual attenuation from original extract (°P) and apparent/actual extract (°P).
attenuation :: Extract -> Extract -> Percentage
attenuation oe ae = 1.0 - ae / oe
