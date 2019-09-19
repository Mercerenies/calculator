
module Data.Calc.Mode where

data ModeInfo = ModeInfo {
      angularMode :: AngularMode,
      exactnessMode :: ExactnessMode
    } deriving (Show, Read, Eq)

data AngularMode = Radians | Degrees
                   deriving (Show, Read, Eq, Ord, Enum)

data ExactnessMode = Floating | Fractional | Symbolic
                     deriving (Show, Read, Eq, Ord, Enum)

defaultMode :: ModeInfo
defaultMode = ModeInfo {
                angularMode = Radians,
                exactnessMode = Fractional
              }
