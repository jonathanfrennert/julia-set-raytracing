module Quaternions (

  Quaternion (..),
  lenH,
  Angles (..),

  ) where

-- | Quaternion datatype.
data Quaternion = H Double Double Double Double
  deriving (Show, Eq)

-- | Instance for quaternion operations.
instance Num Quaternion where
  fromInteger n                 = H (fromIntegral n) 0 0 0
  (H a b c d) * (H a' b' c' d') = H (a*a' - b*b' - c*c' - d*d')
                                    (a*b' + b*a' + c*d' - d*c')
                                    (a*c' - b*d' + c*a' + d*b')
                                    (a*d' + b*c' - c*b' + d*a')
  (H a b c d) + (H a' b' c' d') = H (a+a') (b+b') (c+c') (d+d')
  (H a b c d) - (H a' b' c' d') = H (a-a') (b-b') (c-c') (d-d')
  abs (H a b c d)               = H (sqrt (a*a + b*b + c*c + d*d)) 0 0 0
  signum (H a b c d)            = H (signum a) (signum b) (signum c) (signum d)

-- | Real quaternion component.
rH :: Quaternion -> Double
rH (H a _ _ _) = a

-- | The length of a Quaternion from origin.
lenH :: Quaternion -> Double
lenH = rH.abs

-- | Angles datatype.
data Angles = A Double Double Double Double
  deriving (Show)
