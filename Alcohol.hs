module Alcohol where

import Common
import Aerometer
import DeClerck


-- |Compute alcoholic content by weight (%) from original extract `oe` (°P) and actual extract `ae` (°P).
alcoholByWeightFromExtract :: Extract -> Extract -> Percentage
alcoholByWeightFromExtract oe ae = constrainPercentage $ (oe - ae) / (balling - 0.010665 * oe) / 100.0


-- |Same as `alcoholByWeightFromExtract` with input values derived from:
-- - Original extract is computed from an initial refraction value `ri` and a wort correction `wc` (typically 1.03 to 1.04)
-- - The actual extract is computed via DeClerck from an apparent specific gravity `g`.
alcoholByWeight :: Double -> Refraction -> Gravity -> Percentage
alcoholByWeight wc ri g = alcoholByWeightFromExtract oe ae
  where
    oe = ri / wc
    ae = actualExtract oe $ extractForGravity g


-- |Convert alcohol by weight to alcohol by volume.
alcoholByVolume :: Percentage -> Percentage
alcoholByVolume abw = abw / 0.79336
