module JuliaSet (

  JuliaSet (..),
  while,

  ) where

import Settings (maxIt, limit)
import Quaternions

-- | Julia set datatype.
data JuliaSet = JS
  { x :: Int          -- ^ Horizontal resolution.
  , y :: Int          -- ^ Vertical resolution.
  , low :: Double     -- ^ Lower color gradient value (0-1).
  , high :: Double    -- ^ Highest color gradient value (0-1).
  , c :: Quaternion   -- ^ Quaternion Julia set seed.
  , zoom :: Double    -- ^ Camera Zoom (1-0).
  , angles :: Angles  -- ^ Camera angles (rad) around six planes (XY, XZ, YZ, XW, YW, ZW).
  } deriving (Show)

-- | Double quaternion scalar multiplication.
scH :: Double -> Quaternion -> Quaternion
scH n (H a b c d) = H (n*a) (n*b) (n*c) (n*d)

-- | Optimized square of a quaternion.
sqH :: Quaternion -> Quaternion
sqH (H a b c d) = H (a*a - b*b - c*c - d*d) (2*a*b) (2*a*c) (2*a*d)

-- | Anti-clockwise rotation of a quaternion on the imaginary plane.
rot :: Quaternion -> Angles -> Quaternion
rot (H r i j k) (A c c' s s') = let
                                  r' = -(i * c' + j * s')
                                  i' = r * c' - k * s'
                                  j' = r * s' + k * c'
                                  k' = i * s' - j * c'
                                in
                                  H (-(c * i' + s * j')) (c * r' + s * k') (-c * k' + s * r') (c * j' - s * i')

-- | Inverse of anti-clockwise rotation of a quaternion on the imaginary plane.
invRot :: Quaternion -> Angles -> Quaternion
invRot (H r i j k) (A c' c s' s) = let
                                     r' = -(i * c' + j * s')
                                     i' = r * c' - k * s'
                                     j' = r * s' + k * c'
                                     k' = i * s' - j * c'
                                   in
                                     H (-(c * i' + s * j')) (c * r' + s * k') (-c * k' + s * r') (c * j' - s * i')

-- | Next Quaternion in Julia set iteration and its derivative.
next :: Quaternion -> Angles -> (Quaternion, Quaternion) -> (Quaternion, Quaternion)
next c a (q,q') = (rot (sqH q + c) a, 2 `scH` q * q')

-- | Julia set value below escape threshhold and its derivative (in the context of the Julia set function).
while :: Quaternion -> Angles -> ( (Quaternion, Quaternion), Int) -> (Quaternion, Quaternion)
while c a i@( (q, q'), n) = if (n < maxIt && lenH q < limit)
                              then (while c a (next c a (invRot q a, q'), n+1))
                              else (q,q')
