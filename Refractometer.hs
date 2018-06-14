module Refractometer
  ( RefractometerFormula
  , apparentSG
  , apparentToActualSG
  ) where

import Common
import DeClerck


-- Supported refractometer formulas
data RefractometerFormula = Standard
                          | Kleier
                          | TerrillCubic
                          | TerrillLinear
                          deriving (Enum)


-- |Compute apparent specific gravity (SG) from initial refraction `ri` and final refraction `rf` using the specified formula.
apparentSG :: RefractometerFormula -> Double -> Refraction -> Refraction -> Gravity
apparentSG Standard      = convStandard
apparentSG Kleier        = convKleier
apparentSG TerrillCubic  = convTerrillCubic
apparentSG TerrillLinear = convTerrillLinear


-- |Compute apparent specific gravity according the standard formula.
-- The initial refraction is converted to an extract using wort correction `wc` (typically 1.03 to 1.04)
convStandard :: Double -> Refraction -> Refraction -> Gravity
convStandard wc ri rf =
  1.001843
  - 0.2318474e-2 * ri_corrected
  - 0.7775e-5    * ri_corrected**2
  - 0.34e-7      * ri_corrected**3
  + 0.574e-2     * rf
  + 0.3344e-4    * rf**2
  + 0.86e-7      * rf**3
  where
    ri_corrected = ri / wc


-- |Compute apparent specific gravity according the formula developed by Kleier.
-- The initial refraction is converted to an extract using wort correction `wc` (typically 1.03 to 1.04)
--
-- http://hobbybrauer.de/modules.php?name=eBoard&file=viewthread&tid=11943&page=2#pid129201
convKleier :: Double -> Refraction -> Refraction -> Gravity
convKleier wc ri rf = gravityForExtract fg_apparent
  where
    fg_actual   = (balling * rf - 0.44552 * ri) / (balling * wc - 0.44552)
    fg_apparent = fg_actual * (1.22 + 0.001 * ri) - ((0.22 + 0.001 * ri) * ri)


-- |Compute apparent specific gravity according the new cubic formula of Sean Terrill.
-- The initial and final refraction values are converted to extract values using wort correction `wc` (typically 1.03 to 1.04)
convTerrillCubic :: Double -> Refraction -> Refraction -> Gravity
convTerrillCubic wc ri rf =
  1.0
  - 0.44993e-2 * ri_corrected
  + 0.11774e-1 * rf_corrected
  + 0.27581e-3 * ri_corrected**2
  - 0.12717e-2 * rf_corrected**2
  - 0.728e-5   * ri_corrected**3
  + 0.63293e-4 * rf_corrected**3
  where
    ri_corrected = ri / wc
    rf_corrected = rf / wc


-- |Compute apparent specific gravity according the new linear formula of Sean Terrill.
-- The initial refraction is converted to an extract using wort correction `wc` (typically 1.03 to 1.04)
convTerrillLinear :: Double -> Refraction -> Refraction -> Gravity
convTerrillLinear wc ri rf = 1.0 - 0.85683e-3 * ri_corrected + 0.34941e-2 * rf_corrected
  where
    ri_corrected = ri / wc
    rf_corrected = rf / wc


-- |Convert apparent specific gravity `g` measured for wort with an initial refraction `ri` to the actual specific gravity.
-- The initial refraction is converted to an extract using wort correction `wc` (typically 1.03 to 1.04)
apparentToActualSG :: Double -> Refraction -> Gravity -> Gravity
apparentToActualSG wc ri g = gravityForExtract $ 0.1808 * ri_corrected + 0.8192 * extract
  where
    ri_corrected = ri / wc
    extract      = extractForGravity g
