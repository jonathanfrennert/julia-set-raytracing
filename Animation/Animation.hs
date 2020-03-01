module Animation
  ( Setting (..)
  , frameGen
  ) where

import qualified Data.Array.Repa as R
import qualified Codec.Picture as J
import qualified Raytracing as T
import qualified Settings as S
import JuliaSet (JuliaSet (..))
import Quaternions (Quaternion (..), Angles (..))

-- Frames ---------------------------------------------------------------------

-- | Animation settings datatype.
data Setting = Set
  { x :: Int          -- ^ Horizontal resolution.
  , y :: Int          -- ^ Vertical resolution.
  , fps :: Int        -- ^ Frames per second.
  , time :: Double    -- ^ Video duration (sec).
  , low :: Double     -- ^ Lower color gradient value (0-1).
  , high :: Double    -- ^ Highest color gradient value (0-1).
  , c :: Quaternion   -- ^ Quaternion Julia set seed.
  , zoom :: Double    -- ^ Camera Zoom (1-0).
  } deriving (Show)

-- | Generate frames given the settings.
frameGen :: Setting -> IO ()
frameGen set@(Set _ _ fps time _ _ _ _) = do
  frameLoop set 0 (truncate $ (realToFrac fps) * time)
  putStrLn "\nFrames Rendered."

-- | Frame generation loop.
frameLoop :: Setting -> Int -> Int -> IO ()
frameLoop set n total = if n < total
                          then do
                            frameWrite set n
                            frameLoop set (n+1) total
                          else return ();

-- | Write a frame.
frameWrite :: Setting -> Int -> IO ()
frameWrite (Set hRes vRes fps _ lo hi c z) n = do
  period <- return $ pi * realToFrac n / (60 * realToFrac fps)
  frame  <- R.computeUnboxedP.render $ JS hRes vRes lo hi (seed c period) z (angle period)
  J.writePng ("frames/"++ show n ++ ".png") (repa2juicy frame)

-- | Cosine and sine calculation.
angle :: Double -> Angles
angle a = A (cos a) (cos $ -a) (sin a) (sin $ -a)

-- / Quaternion Julia set seed.
seed :: Quaternion -> Double -> Quaternion
seed (H r i j k) a = H r (cos a * i) (sin a * j) k

-- Repa -----------------------------------------------------------------------

-- | Wrapper of RGB channels for Repa implementation.
type RGB8 = (J.Pixel8, J.Pixel8, J.Pixel8)

-- | Generate the repa array (optimized arrays which allow for multi-core processing).
render :: JuliaSet -> R.Array R.D R.DIM2 RGB8
render js@(JS x y _ _ _ _ a) = R.fromFunction (R.Z R.:. x R.:. y) (T.pixelColor js S.cPos S.lPos)

-- Juicy Pixels ---------------------------------------------------------------

-- | Get image with true color pixels from repa array.
repa2juicy :: R.Array R.U R.DIM2 RGB8 -> J.Image J.PixelRGB8
repa2juicy image = J.generateImage gen width height
  where
    (R.Z R.:. width R.:. height) = R.extent image  -- ^ Get horizontal and vertical resolution from the array.
    gen x y =                                      -- ^ Generate the corresponding color in the array given for the pixel coordinates.
      let (r,g,b) = image R.! (R.Z R.:. x R.:. y)
      in J.PixelRGB8 r g b
