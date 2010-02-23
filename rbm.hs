import Data.Packed.Matrix as M
import Numeric.LinearAlgebra.Instances

-- W: visible to hidden
-- L: visible to visible
-- J: hidden to hidden

type Matrix' = M.Matrix Double

type Visible = Matrix'
type Hidden = Matrix'
type WParam = Matrix'
type LParam = Matrix'
type JParam = Matrix'
type Parameters = (WParam, LParam, JParam)
type Output = Matrix'

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

thd3 :: (a, b, c) -> c
thd3 (a, b, c) = c

energy :: Visible -> Hidden -> Parameters -> Output
energy v h p = vToV - hToH - vToH
  where
    vToV = -0.5 * ((trans v) * lv)
    hToH = 0.5 * ((trans h) * jh)
    vToH = (trans v) * wh
    lv = (snd3 p) * h
    jh = (thd3 p) * h
    wh = (fst3 p) * h
--
--assignProb :: Visible -> Hidden -> Double
--assignProb v h = (recip z) * sum $ exp (-(energy v h))
--  where
--    z = sum $ sum 
