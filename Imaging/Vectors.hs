module Vectors (

  Vector (..),
  lenV,
  dot,
  unitV,
  scV

  ) where

-- | Vector datatype.
data Vector = V Double Double Double
  deriving (Show, Eq)

-- | Instance for vector operations.
instance Num Vector where
  fromInteger n            = V (fromIntegral n) (fromIntegral n) (fromIntegral n)
  (V x y z) * (V x' y' z') = V (x*x'+ y*y' + z*z') 0 0
  (V x y z) + (V x' y' z') = V (x+x') (y+y') (z+z')
  (V x y z) - (V x' y' z') = V (x-x') (y-y') (z-z')
  abs (V x y z)            = V (sqrt (x*x + y*y + z*z)) 0 0
  signum (V x y z)         = V (signum x) (signum y) (signum z)

-- | * Vector units.
iV :: Vector -> Double
iV (V x _ _) = x

-- | The length of a vector from origin.
lenV :: Vector -> Double
lenV = iV.abs

-- | Dot product.
dot :: Vector -> Vector -> Double
dot p q = iV $ p*q

-- | Unit vector.
unitV :: Vector -> Vector
unitV p@(V x y z) = V (x/l) (y/l) (z/l)
  where l = lenV p  -- Magnitude of vector.

-- | Scalar multiplication (for Doubles).
scV :: Double -> Vector -> Vector
scV n (V x y z) = V (n*x) (n*y) (n*z)
