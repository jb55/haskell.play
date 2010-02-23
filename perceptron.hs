import Control.Monad
import System.Random

type FType = Float

defaultThreshold :: (Floating a) => a
defaultThreshold = 0.2

defaultLearnRate :: (Floating a) => a
defaultLearnRate = 0.1

neuronOutput :: (Floating a, Ord a) => a -> [a] -> [a] -> a
neuronOutput threshold inputs weights
  | iw >= 0 = 1
  | otherwise = 0
  where
    iw = (foldl(+) 0 (zipWith (*) inputs weights)) - threshold

sigmoidActivate :: (Floating a, Ord a) => a -> a -> a
sigmoidActivate threshold a
  | t < threshold = 0
  | otherwise = 1
  where
    t = sigmoid a

sigmoid :: (Floating a) => a -> a
sigmoid t = 1 / (1 + (exp 1)**(-t))

adjustWeights :: (Floating a) => [a] -> [a] -> a -> a -> a -> [a]
adjustWeights inputs origWeights learnRate expected actual =
  map delta (zip inputs origWeights)
  where
    e = expected - actual
    delta (i, w) = w + (learnRate * i * e)

step :: (Floating a, Ord a) => [a] -> [a] -> a -> [a]
step inputs weights expected =
  adjustWeights inputs weights defaultLearnRate expected o
  where
    o = neuronOutput defaultThreshold inputs weights 

epoch :: (Floating a, Ord a) => [([a], a)] -> [a] -> ([a], a)
epoch allInputs weights = (newW, delta)
  where
    f w (inputs, expected) = step inputs w expected
    newW = foldl f weights allInputs
    numW = fromIntegral $ length weights
    deltaW = map abs $ zipWith (-) newW weights
    delta = (foldl (+) 0 deltaW) / numW

run :: (Floating a, Ord a) => [([a], a)] -> [a] -> Int -> ([a], Int)
run allInputs weights epochNb
  | delta == 0 = (newWeights, epochNb)
  | otherwise = run allInputs newWeights (epochNb + 1)
  where
    (newWeights, delta) = epoch allInputs weights

initialWeights :: (Random a, Floating a, Ord a) => Int -> IO [a]
initialWeights nb = do
  let interval = randomR (-0.5, -0.5)
  replicateM nb (getStdRandom interval)
   
andGate = [([0,0],0),([0,1],0),([1,0],0),([1,1],1)] :: [([FType], FType)]
orGate = [([0,0],0),([0,1],1),([1,0],1),([1,1],1)] :: [([FType], FType)]

neuron = neuronOutput defaultThreshold

trainedWeights :: (Random a, Floating a, Ord a) => [([a], a)] -> IO [a]
trainedWeights inputs =
  do
    w <- initialWeights $ inLen 
    let (ws, i) = run inputs w 1
    return ws
  where
    inLen = length $ fst $ head inputs

test :: IO ()
test = do
  w <- initialWeights 2
  let (ws, i) = run andGate w 1
  print (ws, i)
  return ()

