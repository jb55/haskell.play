import Data.Packed.Matrix as M
import Numeric.LinearAlgebra.Instances

-- W: visible to hidden
-- L: visible to visible
-- J: hidden to hidden

type FType = Double
type Probability = FType
type Matrix' = M.Matrix FType

type Visible = Matrix'
type Hidden = Matrix'
type WParam = Matrix'
type LParam = Matrix'
type JParam = Matrix'
type Output = Matrix'

logistic :: (Floating a) => a -> a
logistic x = recip (1 + exp (-x))

energy :: Visible -> Hidden -> WParam -> Output
energy v h w = negate $ (trans v) * (w * h)
