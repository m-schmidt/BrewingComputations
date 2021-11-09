module Hops where

import Common


-- |Hop utilization percentage according Glenn Tinseth after boiling time `t` in wort of gravity `g` (intended for umbels).
hopUtilization :: Gravity -> Duration -> Percentage
hopUtilization g t = bigness g * boiltime t
  where
    -- reduced utilization due to higher wort gravities
    bigness g  = 1.65 * 0.125e-3 ** (g - 1)
    -- change in utilization over time
    boiltime t = (1 - exp (-0.04 * t/60.0)) / 4.15


-- |Utilization factor for hop pellets compared to umbels
utilizationPellet :: Bool -> Double
utilizationPellet False = 1.0
utilizationPellet True  = 1.1


-- |Utilization factor for first wort hop
utilizationFirstWort :: Bool -> Double
utilizationFirstWort False = 1.0
utilizationFirstWort True  = 0.9


-- |Compute IBU for hop amount `w` (g) with alpha value `a` (%) and utilization `u` (%) for wort volume `v` (l).
bitterness :: Volume -> Percentage -> Percentage -> Weight -> IBU
bitterness v a u w = 1000.0 * w * a * u / v


-- |Compute amount of hop (g) such that a wort of volume `v` (l) reaches bitterness `ibu` with a hop of alpha value `a` (%) and utilization `u` (%).
amount :: IBU -> Volume -> Percentage -> Percentage -> Weight
amount ibu v a u = ibu * v / a / u / 1000.0


-- |Compute hop amounts (g) such that a wort of volume `v` (l) reaches a total bitterness `ibu` when hops with weight distribution `hs`, alpha values `as`, and utilizations `us` (all %) are added.
hopAmountsByWeight :: IBU -> Volume -> [Percentage] -> [Percentage] -> [Percentage] -> [Weight]
hopAmountsByWeight ibu v as us hs = scale (amount0_initial * ibu_scale) amounts_initial
  where
    -- amount to fulfill desired bitterness with only the first hop
    amount0_initial = amount ibu v (head as) (head us)
    -- scaled hop amounts based on amount0_initial
    amounts_initial = scale amount0_initial hs
    -- total ibu based on amount0_initial
    total_ibu_initial = sum $ zipWith3 (bitterness v) as us amounts_initial
    -- scale factor to reach desired total ibu
    ibu_scale = ibu / total_ibu_initial
    -- helper to scale a list `xs` such that the head element becomes `v`
    scale v xs@(x0:_) = map (\x -> x * v/x0) xs
    scale _ _ = undefined


-- |Compute hop amounts (g) such that a wort of volume `v` (l) reaches a total bitterness `ibu` when hops with ibu distribution `is`, alpha values `as`, and utilizations `us` (all %) are added.
hopAmountsByIBU :: IBU -> Volume -> [Percentage] -> [Percentage] -> [Percentage] -> [Weight]
hopAmountsByIBU ibu v = zipWith3 (\a u i -> amount (ibu * i) v a u)
