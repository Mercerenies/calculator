
module Data.Calc.Mode where

data ModeInfo = ModeInfo {
      angularMode :: AngularMode
    } deriving (Show, Read, Eq)

data AngularMode = Radians | Degrees
                   deriving (Show, Read, Eq, Ord, Enum)

defaultMode :: ModeInfo
defaultMode = ModeInfo {
                angularMode = Radians
              }
