module Main where

import System.Environment (getArgs)
import System.Process
import qualified Animation as A
import qualified Settings as S
import Quaternions (Quaternion (..))

-- Main -----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  argCheck args
  (filename, choice) <- parse args
  setting <- interpret choice
  prettySpecs setting
  A.frameGen setting
  ffmpegSetting <- ffmpegFormat filename setting
  runCommand ffmpegSetting
  putStrLn "Video Rendered.\n"

-- Errors ---------------------------------------------------------------------

-- | Check settings input.
settingsCheck :: String -> IO ()
settingsCheck x
  | x == "default" = putStrLn "Settings check done: default"
  | x == "custom"  = putStrLn "Settings check done: custom"
  | otherwise      = error "Invalid second argument (default/custom)."

-- | General argument checks (number of arguments, extension check and settings check).
argCheck :: [String] -> IO ()
argCheck path = case path of
                 []     -> error "No arguments - VideoGen.exe <Video name> (default/custom)"
                 [x]    -> error "No second argument (default/custom)."
                 (x:xs) -> do settingsCheck.head $ xs;

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
userInput :: IO A.Setting
userInput = do
  x <- questionInt "\n(1/15) Horizontal resolution? (Int)"
  y <- questionInt "\n(2/15) Vertical resolution? (Int)"
  fps <- questionInt "\n(2/15) FPS? (Int)"
  time <- questionDouble "\nVideo length (sec)?"
  r <- questionDouble "\n(3/15) r(c)? (0-1)"
  i <- questionDouble "\n(4/15) i(c)? (0-1)"
  j <- questionDouble "\n(5/15) j(c)? (0-1)"
  k <- questionDouble "\n(6/15) k(c)? (0-1)"
  lo <- questionDouble "\n(7/15) Gradient low? (0-1)"
  hi <- questionDouble "\n(8/15) Gradient high? (0-1)"
  z <- questionDouble "\n(9/15) Camera Zoom? (1-0)"
  return $ A.Set x y fps time lo hi (H r i j k) z

-- | Returns a default julia set or a custom julia set.
interpret :: String -> IO A.Setting
interpret "default" = return $ A.Set S.hRes S.vRes S.fps S.time S.lo S.hi S.c S.z  -- ^ Default settings.
interpret "custom"  = userInput                                                    -- ^ Custom user input.

-- Pretty specifications ------------------------------------------------------

-- | To show specifications on command prompt.
prettySpecs :: A.Setting -> IO ()
prettySpecs (A.Set hRes vRes fps time lo hi c z) = putStrLn.concat $ ["\n-- Specifications ------------------------------------------------------------------",
                                                                    "\n\nResolution = ",show hRes,"x",show vRes,
                                                                      "\nFPS = ", show fps,
                                                                      "\nVideo Length (sec) = ", show time,
                                                                      "\nSeed = ", show c,
                                                                      "\nGradient = {",show lo,",",show hi,"}",
                                                                      "\nCamera Zoom = ",show z,
                                                                    "\n\n------------------------------------------------------------------------------------"]

-- ffmpeg ---------------------------------------------------------------------

ffmpegFormat :: String -> A.Setting -> IO String
ffmpegFormat filename (A.Set x y f t _ _ _ _) = return.concat $ ["ffmpeg "
                                                                , fps
                                                                , "-f image2 ", res
                                                                , "-i frames/%d.png "
                                                                , "-vcodec libx264 "
                                                                , "-crf 15 "
                                                                , "-pix_fmt yuv420p "
                                                                , filename, ".mp4"]
  where fps     = concat ["-framerate ",show f," "]
        res     = concat ["-s ",show x,"x",show y," "]
