import Data.List
-- W: visible to hidden
-- L: visible to visible
-- J: hidden to hidden


-- Yeah... I'm switching to a matrix lib

type Matrx = [[Double]]

type Visible = Matrix
type Hidden = Matrix
type WParam = Matrix
type LParam = Matrix
type JParam = Matrix
type Parameters = (WParam, LParam, JParam)
type Output = Matrix

energy :: Visible -> Hidden -> Parameters -> Output
energy v h p = zipWith3 (+) vToV hToH vToH
  where
    f = (\x y -> map (*(-0.5)) (x `transpose` y))
    vToV = f v lv
    hToH = f h jh
    vToH = v `transpose` wh
    lv = zipWith (*) (fst3 p) v
    jh = zipWith (*) (snd3 p) h
    wh = zipWith (*) (thd3 p) h
--
--assignProb :: Visible -> Hidden -> Double
--assignProb v h = (recip z) * sum $ exp (-(energy v h))
--  where
--    z = sum $ sum 
