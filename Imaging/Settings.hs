module Settings (

  hRes, vRes,
  c, a, maxIt, limit,
  lo, hi,
  cPos, z,
  lPos, shine, spec,
  rad2,

  ) where

import Vectors (Vector (..))
import Quaternions (Quaternion (..), Angles (..))

-- * Resolution settings ------------------------------------------------------

-- | Horizontal resolution.
hRes :: Int
hRes = 640

-- | Vertical Resolution.
vRes :: Int
vRes = 480

-- * Julia set settings -------------------------------------------------------

-- | Default Julia set seed.
c :: Quaternion
c = H 0.45 0.24 (-0.52) 0.39

-- | Default Julia set angles around the six planes (XY, XZ, YZ, XW, YW, ZW).
a :: Double
a = 0

-- | Number of iterations for Julia set.
maxIt :: Int
maxIt = 10

-- | Maximum threshhold (<= 2 for grainy image).
limit :: Double
limit = 15

-- * Coloring settings --------------------------------------------------------

-- | Color gradient low (0-1).
lo :: Double
lo = 0

-- | Color gradient high (0-1).
hi :: Double
hi = 1

-- * Camera settings ----------------------------------------------------------

-- | Camera position.
cPos :: Vector
cPos = V 0 0 7

-- | Camera zoom (1-0), 1 is normal zoom.
z :: Double
z = 1

-- * Lighting settings --------------------------------------------------------

-- | Light position.
lPos :: Vector
lPos = V 1 1 7

-- | Shininess.
shine :: Int
shine = 10

-- | Specularity.
spec :: Double
spec = 0.6

-- Optimization ---------------------------------------------------------------

-- | Radius of the bounding sphere squared.
rad2 :: Double
rad2 = 3
