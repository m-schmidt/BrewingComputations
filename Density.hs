--
-- The code in this module is largely based on the brewing calculations from Fabian Kaiser
-- http://fabier.de/biercalcs.html
--
-- The density data values are taken from:
-- "Zuckertechniker-Taschenbuch", Albert Bartens Verlage, Berlin, 1966, 7. Auflage
--
-- Minor improvements of some data values according:
-- "Ullmann-Zuckertechnologie", Band 24, Dr. Hubert Schiweck, Süddeutsche Zucker AG, Mannheim.
--

module Density
  ( wortDensity
  , adjustExtract
  , waterDensity
  , adjustVolume
  ) where

import Common
import Lagrange
import Data.List


-- |Density values (g/ml)
densities :: [[Density]]
densities = transpose
              --  0°P      5°P      10°P     15°P     20°P     25°P     30°P     35°P     40°P
              [ [ 1.00020, 1.02040, 1.04180, 1.06320, 1.08510, 1.10880, 1.13230, 1.15740, 1.18400 ]  --   0°C (extrapolated)
              , [ 0.99970, 1.01960, 1.04020, 1.06140, 1.08350, 1.10640, 1.13010, 1.15470, 1.18020 ]  --  10°C
              , [ 0.99820, 1.01780, 1.03814, 1.05910, 1.08096, 1.10350, 1.12698, 1.15130, 1.17645 ]  --  20°C
              , [ 0.99570, 1.01510, 1.03529, 1.05610, 1.07766, 1.10000, 1.12325, 1.14730, 1.17231 ]  --  30°C
              , [ 0.99220, 1.01160, 1.03156, 1.05220, 1.07366, 1.09580, 1.11888, 1.14280, 1.16758 ]  --  40°C
              , [ 0.98810, 1.00720, 1.02713, 1.04770, 1.06898, 1.09100, 1.11395, 1.13770, 1.16238 ]  --  50°C
              , [ 0.98320, 1.00230, 1.02207, 1.04240, 1.06365, 1.08560, 1.10847, 1.13210, 1.15675 ]  --  60°C
              , [ 0.97780, 0.99680, 1.01648, 1.03680, 1.05790, 1.07980, 1.10257, 1.12620, 1.15073 ]  --  70°C
              , [ 0.97180, 0.99080, 1.01039, 1.03060, 1.05169, 1.07350, 1.09626, 1.11980, 1.14432 ]  --  80°C
              , [ 0.96530, 0.98420, 1.00380, 1.02400, 1.04500, 1.06690, 1.08960, 1.11300, 1.13750 ]  --  90°C
              , [ 0.95910, 0.97800, 0.99750, 1.01760, 1.03860, 1.06060, 1.08320, 1.10650, 1.13090 ]  --  99°C
              ]


-- |Temperatures of density data points.
densTemperatures :: [Temperature]
densTemperatures = reverse $ 99:[90,80..0]


-- |Extract amounts of density data points.
densExtracts :: [Extract]
densExtracts = [0,5..40]


-- |Density values for temperature `t`.
densitiesAtTemperature :: Temperature -> [Density]
densitiesAtTemperature t = map (interpolate t densTemperatures) densities


-- |Compute interpolated density of wort at temperature `t` and with extract amount `e`.
wortDensity :: Temperature -> Extract -> Density
wortDensity t e = interpolate e densExtracts $ densitiesAtTemperature t


-- |Adjust extract amount `e` measured at temperature `t` with an aerometer calibrated to temperature `c`.
adjustExtract :: Temperature -> Temperature -> Extract -> Extract
adjustExtract c t e | c == t    = e
                    | otherwise = interpolate dc ds densExtracts
  where
    dc = wortDensity c e
    ds = densitiesAtTemperature t


-- |Compute density of pure water (g/ml) at temperature `t`.
-- Approximation formula taken from http://www.wissenschaft-technik-ethik.de/downloads/wasserdichteberechnung_v3-0.xls
waterDensity :: Temperature -> Density
waterDensity t =
  (999.83952
  + 16.952577     * t
  - 7.9905127e-3  * t**2
  - 4.6241757e-5  * t**3
  + 1.0584601e-7  * t**4
  - 2.8103006e-10 * t**5
  ) / (1 + 0.016887236 * t) / 1000


-- |Adjust volume `v1` measured at temperature `t1` to temperature `t2`.
adjustVolume :: Temperature -> Temperature -> Double -> Double
adjustVolume t1 t2 v1 | t1 == t2  = v1
                      | otherwise = d1 * v1 / d2
  where
    d1 = waterDensity t1
    d2 = waterDensity t2
