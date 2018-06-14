--
-- The code in this module is based on:
--
--  - Description "Wasseraufbereitung" on Maischemalzundmehr.de
--    https://www.maischemalzundmehr.de/index.php?inhaltmitte=toolswasserrechner
--
--  - Braumagazin Frühjahr 2015:
--    http://braumagazin.de/article/von-der-wasseranalyse-zum-brauwasser/
--

module Alkalinity
  ( MineralContentUnit
  , carbonateAlkalinity
  , calciumAlkalinity
  , magnesiumAlkalinity
  , residualAlkalinity
  , mmolForResidualAlkalinity
  ) where

import Common


-- |Common units for mineral content of water
data MineralContentUnit = Unit_mmolPerLiter      -- ^ mmol/l
                        | Unit_mgPerLiter        -- ^ mg/l
                        | Unit_ppm               -- ^ ppm
                        | Unit_mvalPerLiter      -- ^ mval/l
                        | Unit_germanAlkalinity  -- ^ °dH
                        | Unit_frenchAlkalinity  -- ^ °fH
                        deriving (Enum)


-- Molecular and atomic masses

-- |Hydrogen carbonate (g/mol)
molecularMass_HCO3 = 61.0168

-- |Calcium oxide (g/mol)
molecularMass_CaO = 56.0774

-- |Calcium carbonate (g/mol)
molecularMass_CaCO3 = 100.0869

-- |Calcium (u)
atomicMass_Ca = 40.078

-- |Magnesium (u)
atomicMass_Mg = 24.305


-- Derived factors for conversion

-- |mmol/l of CaO corresponding to 1°dH (about 0.1783)
mmol_CaO = 10 / molecularMass_CaO

-- |mmol/l of HCO3 corresponding to 1°dH (about 0.3566)
mmol_HCO3 = 2 * mmol_CaO

-- |mmol/l of CaCO3 corresponding to 1°fH (about 0.0999)
mmol_CaCO3 = 10 / molecularMass_CaCO3


-- |Unit conversion for carbonate alkalinity (°dH).
carbonateAlkalinity :: Double -> MineralContentUnit -> Double
carbonateAlkalinity hco3 unit =
  case unit of
    Unit_mmolPerLiter     -> hco3 / mmol_HCO3
    Unit_mgPerLiter       -> hco3 / molecularMass_HCO3 / mmol_HCO3
    Unit_ppm              -> hco3 / molecularMass_HCO3 / mmol_HCO3
    Unit_mvalPerLiter     -> hco3 / mmol_HCO3
    Unit_germanAlkalinity -> hco3
    Unit_frenchAlkalinity -> hco3 * mmol_CaCO3 / mmol_CaO


-- |Unit conversion for calcium alkalinity (°dH).
calciumAlkalinity :: Double -> MineralContentUnit -> Double
calciumAlkalinity ca unit =
  case unit of
    Unit_mmolPerLiter     -> ca / mmol_CaO
    Unit_mgPerLiter       -> ca / atomicMass_Ca / mmol_CaO
    Unit_ppm              -> ca / atomicMass_Ca / mmol_CaO
    Unit_mvalPerLiter     -> ca / mmol_HCO3
    Unit_germanAlkalinity -> ca
    Unit_frenchAlkalinity -> ca * mmol_CaCO3 / mmol_CaO


-- |Unit conversion for magnesium alkalinity (°dH).
magnesiumAlkalinity :: Double -> MineralContentUnit -> Double
magnesiumAlkalinity mg unit =
  case unit of
    Unit_mmolPerLiter     -> mg / mmol_CaO
    Unit_mgPerLiter       -> mg / atomicMass_Mg / mmol_CaO
    Unit_ppm              -> mg / atomicMass_Mg / mmol_CaO
    Unit_mvalPerLiter     -> mg / mmol_HCO3
    Unit_germanAlkalinity -> mg
    Unit_frenchAlkalinity -> mg * mmol_CaCO3 / mmol_CaO


-- |Compute residual alkalinity from other alkalinities (all values in °dH).
residualAlkalinity alkalinityHCO alkalinityCA alkalinityMG =
  alkalinityHCO - ((alkalinityCA + 0.5 * alkalinityMG) / 3.5)


-- |Conversion from °dH to mmol/l for residual alkalinity.
mmolForResidualAlkalinity a = a * mmol_HCO3
