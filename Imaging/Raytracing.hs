{-# LANGUAGE TypeOperators #-}

module Raytracing (pixelColor) where

import qualified Data.Array.Repa as R
import Data.Word (Word8)
import Data.Array (Array,array,(!))
import JuliaSet
import Vectors
import Quaternions
import Settings (spec, shine, rad2)

-- | Projection plane direction vector given the pixel coordinates.
projection :: JuliaSet -> Vector -> Double -> Double -> Vector
projection js@(JS hRes vRes _ _ _ z _) cPos x y = unitV $ V (normX * rad) (normY * rad) rad - cPos
  where rad = z * sqrt rad2                    -- ^ ~ Size of projection plane.
        normX = 2 * x / fromIntegral hRes - 1  -- ^ Convert horizontal resolution for juicy pixels image format (-1,1).
        normY = 2 * y / fromIntegral vRes - 1  -- ^ Convert vertical resolution for juicy pixels image format (-1,1).

-- | If there is an intersection with the bounding sphere, the intersection coordinates are given, else the camera position.
sphereIntersect :: Vector -> Vector -> Vector
sphereIntersect cPos dir = if delta >= 0 then cPos + (sc+0.003) `scV` dir else cPos
  where b     = cPos `dot` dir                -- ^ b value for quadratic equation to determine line-sphere interesection.
        c     = cPos `dot` cPos - rad2        -- ^ c value for quadratic equation to determine line-sphere interesection.
        delta = sqrt (b*b - c)                -- ^ delta value for quadratic equation to determine line-sphere interesection.
        sc    = min (-b - delta) (delta - b)  -- ^ Scalar multiple for direction vector to sphere intersect from camera position.

-- | Lower bound estimate for distance from position to the Julia set.
estimate :: Quaternion -> Angles -> Vector -> Double
estimate c a (V r i j) = 0.5 * q * (log q) / lenH q'
  where (z,q')        = while c a ( (H r i j 0, H 1 0 0 0), 0)  -- ^ Quaternion Julia set value and its corresponding derivative.
        q             = lenH z                                  -- ^ Magnitude of quaternion Julia set value.

-- | If there is a Julia set intersection, the intersection coordinates are given, else the camera position.
jsIntersect :: Quaternion -> Angles -> Vector -> Vector -> Vector -> Vector
jsIntersect c a cPos dir pos = if bound < 0.001
                                 then pos
                                 else (if pos `dot` pos < rad2
                                         then jsIntersect c a cPos dir (pos + bound `scV` dir)
                                         else cPos)
  where bound = estimate c a pos  -- ^ Lower bound estimate of position to quaternion Julia set.


-- | The sphere intersect and the Julia set intersect of the given direction vector.
getProperties :: JuliaSet -> Vector -> Vector -> (Vector, Vector)
getProperties js@(JS _ _ _ _ c _ a) cPos dir = (sPos, if sPos == cPos then cPos else rPos)
  where sPos = sphereIntersect cPos dir       -- ^ Sphere intersect of point.
        rPos = jsIntersect c a cPos dir sPos  -- ^ Julia set intersect of point.

-- | Direction vectors for subpixels for supersampling anti-aliasing.
subpixels :: JuliaSet -> Vector -> Double -> Double -> [Vector]
subpixels js cPos x y = [ projection js cPos (x+h) (y+v) | h <- [-1/4,1/4], v <- [-1/4,1/4]]

-- | Surface normal for position on the Julia set.
norm :: Quaternion -> Angles -> Vector -> Vector
norm c a (V x y z) = V normX normY normZ
  where normX = estimate c a (V (x+0.001) y z) - estimate c a (V (x-0.001) y z)  -- ^ X normal.
        normY = estimate c a (V x (y+0.001) z) - estimate c a (V x (y-0.001) z)  -- ^ Y normal.
        normZ = estimate c a (V x y (z+0.001)) - estimate c a (V x y (z-0.001))  -- ^ Z normal.

-- | Color palette (rainbow).
palette :: Array Int Vector
palette = array (0,599) (concat [red,yel,gre,tur,blu,pur])
  where
    red = [(truncate $ i, V 1 (i/100) 0) | i <- [0..99]]                   -- ^ Red vectors.
    yel = [(truncate $ i + 100, V ((100 - i) / 100) 1 0) | i <- [0..99]]   -- ^ Yellow vectors.
    gre = [(truncate $ i + 200, V 0 1 (i/100)) | i <- [0..99]]             -- ^ Green vectors.
    tur = [(truncate $ i + 300, V 0 ((100 - i) / 100) 1) | i <- [0..99]]   -- ^ Turquoise vectors.
    blu = [(truncate $ i + 400, V (i/100) 0 1) | i <- [0..99]]             -- ^ Blue vectors.
    pur = [(truncate $ i + 500, V 1 0 ((100 - i)  / 100)) | i <- [0..99]]  -- ^ Purple vectors.

-- | Base coloring (Distance estimation).
base :: Double -> Double -> Vector -> Vector
base lo hi rPos@(V x y z) = palette ! (floor $ 599 * (lo + (hi - lo)*(lenV rPos/ sqrt rad2)))

-- | Phong illumination.
illumination :: Quaternion -> Vector -> Vector -> Vector -> Vector -> Vector -> Vector
illumination c cPos lPos nDir rPos col = ((max nDl 0) + spec * (max (c' `dot` r) 0) ^ shine) `scV` col
  where l   = unitV $ lPos - rPos    -- ^ Light direction.
        c'  = unitV $ cPos - rPos    -- ^ Camera direction.
        n   = unitV $ nDir           -- ^ Normal direction.
        nDl = n `dot` l              -- ^ Dot product of normal direction and light direction.
        r   = l - (2 * nDl) `scV` n  -- ^ Light reflection direction.

-- | Subpixel color.
subpixelColor :: JuliaSet -> Vector -> Vector -> Vector -> Vector -> Vector
subpixelColor js@(JS _ _ lo hi c _ a) cPos lPos sPos rPos = if sPos == cPos || rPos == cPos
                                                              then V 0 0 0                                                                              -- ^ Background colour.
                                                              else shader c a cPos lPos nDir rPos.illumination c cPos lPos nDir rPos $ base lo hi rPos  -- ^ Julia set coloring.
  where nDir = norm c a rPos  -- ^ Normal direction.

-- | Mean calculation for RGB vectors.
mean :: [Vector] -> Vector
mean xs = (1 / (realToFrac.length $ xs)) `scV` sum xs

-- | Supersampling anti-aliasing.
antiAliasing :: JuliaSet -> Vector -> Vector -> Double -> Double -> Vector
antiAliasing js@(JS _ _ _ _ c _ _) cPos lPos x y = mean.map (\(sPos, rPos) -> subpixelColor js cPos lPos sPos rPos) $ props
  where dirs  = subpixels js cPos x y             -- ^ Subpixel direction vectors.
        props = map (getProperties js cPos) dirs  -- ^ Bounding sphere intersection and Julia set intersection for subpixels.

-- | Simple shadow rendering.
shader :: Quaternion -> Angles -> Vector -> Vector -> Vector -> Vector -> Vector -> Vector
shader c a cPos lPos nDir rPos col = if jsIntersect c a cPos (unitV $ lPos - rPos) (rPos + 0.002 `scV` (unitV $ lPos - rPos)) == cPos
                                       then col            -- ^ No shading.
                                       else 0.4 `scV` col  -- ^ Hard shading.

-- | Conversion from floats (0-1) to PixelRGB8 (0-255).
v2rgb :: Vector -> (Word8, Word8, Word8)
v2rgb (V r g b) = (d2w r, d2w g, d2w b)
  where d2w n = truncate $ 255 * n  -- ^ Normalize the double (0-1) value.

-- | (Shadows + Illumination + SSAAX4 + Palette) Coloring.
pixelColor :: JuliaSet -> Vector -> Vector -> (R.Z R.:. Int R.:. Int) -> (Word8, Word8, Word8)
pixelColor js cPos lPos (R.Z R.:. x R.:. y) = if fst prop == cPos || rPos == cPos
                                                then (0, 0, 0)
                                                else v2rgb $ antiAliasing js cPos lPos x' y'
  where x'   = realToFrac x               -- ^ X coordinate for division.
        y'   = realToFrac y               -- ^ Y coordinate for division.
        dir  = projection js cPos x' y'   -- ^ Projection direction vector for point.
        prop = getProperties js cPos dir  -- ^ Sphere intersect and Julia set intersect for the point.
        rPos = snd prop                   -- ^ Julia set intersect (has multiple calls).
