{-# LANGUAGE TypeOperators #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Array.Repa as R
import qualified Codec.Picture as J
import Raytracing
import JuliaSet (JuliaSet (..))
import Quaternions (Quaternion (..), Angles (..))
import qualified Settings as S

-- Main -----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  argCheck args
  (fileName, setting) <- parse args
  juliaSet <- interpret setting
  prettySpecs juliaSet
  image  <- R.computeUnboxedP (render juliaSet)
  J.writeTiff ("images/" ++ fileName) (repa2juicy image)
  putStrLn "\nImage Rendered.\n"

-- Errors ---------------------------------------------------------------------

-- | Check .tiff extension.
extCheck :: String -> IO ()
extCheck x
  | drop (length x - 5) x /= ".tiff" = error "No .tiff extension on first argument."
  | otherwise                        = putStrLn "Extension check done"

-- | Check settings input.
settingsCheck :: String -> IO ()
settingsCheck x
  | x == "default" = putStrLn "Settings check done: default"
  | x == "custom"  = putStrLn "Settings check done: custom"
  | otherwise      = error "Invalid second argument (default/custom)."

-- | General argument checks (number of arguments, extension check and settings check).
argCheck :: [String] -> IO ()
argCheck path = case path of
                 []     -> error "No arguments - ImageGen.exe (<name>.tiff) (default/custom)"
                 [x]    -> error "No second argument (default/custom)."
                 (x:xs) -> do extCheck x; settingsCheck.head $ xs;

-- Parser ---------------------------------------------------------------------

-- | Parse arguments.
parse :: [String] -> IO (String,String)
parse xs = return (xs !! 0, xs !! 1)

-- Custom Settings ------------------------------------------------------------

-- | Questions with Int type answers.
questionInt :: String -> IO Int
questionInt prompt = do
  putStrLn prompt
  ans <- getLine
  return (read ans::Int)

-- | For Questions with Double type answers.
questionDouble :: String -> IO Double
questionDouble prompt = do
  putStrLn prompt
  ans <- getLine
  return (read ans::Double)

-- | Custom settings user input.
userInput :: IO JuliaSet
userInput = do
  x <- questionInt "\n(1/15) Horizontal resolution?"
  y <- questionInt "\n(2/15) Vertical resolution?"
  r <- questionDouble "\n(3/15) r(c)?"
  i <- questionDouble "\n(4/15) i(c)?"
  j <- questionDouble "\n(5/15) j(c)?"
  k <- questionDouble "\n(6/15) k(c)?"
  lo <- questionDouble "\n(7/15) Gradient low? (0-1)"
  hi <- questionDouble "\n(8/15) Gradient high? (0-1)"
  z <- questionDouble "\n(9/15) Camera Zoom?"
  a <- questionDouble "\n(10/15) Imaginary plane rotation? (rad)"
  return $ JS x y lo hi (H r i j k) z (A (cos $ a / 2) (cos $ -a / 2) (sin $ a / 2) (sin $ -a / 2))

-- | Returns a default julia set or a custom julia set.
interpret :: String -> IO JuliaSet
interpret "default" = return $ JS S.hRes S.vRes S.lo S.hi S.c S.z (A (cos $ S.a / 2) (cos $ -S.a / 2) (sin $ S.a / 2) (sin $ -S.a / 2))  -- ^ Default settings.
interpret "custom"  = userInput                                                                                                          -- ^ Custom user input.

-- Pretty specifications --------------------------------------------------------

-- | To show specifications on command prompt.
prettySpecs :: JuliaSet -> IO ()
prettySpecs (JS hRes vRes lo hi c z (A a _ _ _)) = putStrLn.concat $ ["\n-- Specifications ------------------------------------------------------------------",
                                                             "\n\nResolution = ",show hRes,"x",show vRes,
                                                               "\nSeed = ", show c,
                                                               "\nGradient = {",show lo,",",show hi,"}",
                                                               "\nCamera Zoom = ",show z,
                                                               "\nImaginary Plane Angle = ",show (2 * acos a),
                                                             "\n\n------------------------------------------------------------------------------------"]

-- Repa -----------------------------------------------------------------------

-- | Wrapper of RGB channels for Repa implementation.
type RGB8 = (J.Pixel8, J.Pixel8, J.Pixel8)

-- | Generate the repa array (optimized arrays which allow for multi-core processing).
render :: JuliaSet -> R.Array R.D R.DIM2 RGB8
render js@(JS x y _ _ _ _ a) = R.fromFunction (R.Z R.:. x R.:. y) (pixelColor js S.cPos S.lPos)

-- Juicy Pixels ---------------------------------------------------------------

-- | Get image with true color pixels from repa array.
repa2juicy :: R.Array R.U R.DIM2 RGB8 -> J.Image J.PixelRGB8
repa2juicy image = J.generateImage gen width height
  where
    (R.Z R.:. width R.:. height) = R.extent image  -- ^ Get horizontal and vertical resolution from the array.
    gen x y =                                      -- ^ Generate the corresponding color in the array given for the pixel coordinates.
      let (r,g,b) = image R.! (R.Z R.:. x R.:. y)
      in J.PixelRGB8 r g b
