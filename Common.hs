module Common where

-- |Unit aliases
type Density     = Double   -- ^ (g/ml)
type Temperature = Double   -- ^ (°C)
type Duration    = Double   -- ^ (s, seconds)
type Extract     = Double   -- ^ (°P or g/100g)
type Gravity     = Double   -- ^ (SG, Specific Gravity)
type Refraction  = Double   -- ^ (°Bx, Brix)
type Percentage  = Double   -- ^ (%, i.e. 1.0 for 100%)
type Volume      = Double   -- ^ (l, liters)
type Weight      = Double   -- ^ (g)
type IBU         = Double   -- ^ (ibu)


-- |Balling's constant
balling = 2.0665

-- |Constrain a percentage to the interval [0.0..1.0].
constrainPercentage :: Percentage -> Percentage
constrainPercentage = max 0.0 . min 1.0
