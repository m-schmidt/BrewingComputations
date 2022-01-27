module Refractometer
  ( RefractometerFormula
  , apparentSG
  , apparentToActualSG
  ) where

import Common
import DeClerck


-- Supported refractometer formulas
data RefractometerFormula = Standard
                          | NovotnyLinear
                          | NovotnyQuadratic
                          | TerrillCubic
                          | TerrillLinear
                          deriving (Enum)


-- |Compute apparent specific gravity (SG) from initial refraction `ri` and final refraction `rf` using the specified formula.
apparentSG :: RefractometerFormula -> Double -> Refraction -> Refraction -> Gravity
apparentSG Standard         = convStandard
apparentSG NovotnyLinear    = convNovotnyLinear
apparentSG NovotnyQuadratic = convNovotnyQuadratic
apparentSG TerrillCubic     = convTerrillCubic
apparentSG TerrillLinear    = convTerrillLinear


-- |Compute apparent specific gravity according the standard formula.
-- |Formula according Louis K. Bonham: "The Use of Handheld Refractometers by Homebrewers" in the Zymurgy January/February 2001
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


-- |Compute apparent specific gravity according the linear formula by Petr Novotný
-- |http://www.diversity.beer/2017/01/pocitame-nova-korekce-refraktometru.html
convNovotnyLinear :: Double -> Refraction -> Refraction -> Gravity
convNovotnyLinear wc ri rf =
  1.0
  - 0.2349e-2 * ri_corrected
  + 0.6276e-2 * rf_corrected
  where
    ri_corrected = ri / wc
    rf_corrected = rf / wc


-- |Compute apparent specific gravity according the quadratic formula by Petr Novotný
-- |http://www.diversity.beer/2017/01/pocitame-nova-korekce-refraktometru.html
convNovotnyQuadratic :: Double -> Refraction -> Refraction -> Gravity
convNovotnyQuadratic wc ri rf = gravityForExtract fg_apparent
  1.0
  + 0.1335e-4 * ri_corrected**2
  - 0.3239e-4 * ri_corrected * rf_corrected
  + 0.2916e-4 * rf_corrected**2
  - 0.2421e-2   * ri_corrected
  + 0.6219e-2   * rf_corrected
  where
    ri_corrected = ri / wc
    rf_corrected = rf / wc


-- |Compute apparent specific gravity according the new cubic formula of Sean Terrill.
-- |http://seanterrill.com
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
-- |http://seanterrill.com
convTerrillLinear :: Double -> Refraction -> Refraction -> Gravity
convTerrillLinear wc ri rf = 1.0 - 0.85683e-3 * ri_corrected + 0.34941e-2 * rf_corrected
  where
    ri_corrected = ri / wc
    rf_corrected = rf / wc


-- |Convert apparent specific gravity `g` measured for wort with an initial refraction `ri` to the actual specific gravity.
apparentToActualSG :: Double -> Refraction -> Gravity -> Gravity
apparentToActualSG wc ri g = gravityForExtract $ 0.1808 * ri_corrected + 0.8192 * extract
  where
    ri_corrected = ri / wc
    extract      = extractForGravity g
