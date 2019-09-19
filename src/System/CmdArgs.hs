
module System.CmdArgs(parseArgv, parseArgv') where

import Data.Calc.Mode

import System.Console.GetOpt

options :: [OptDescr (ModeInfo -> ModeInfo)]
options = [
 Option ['d'] ["degrees"] (NoArg $ \m -> m { angularMode = Degrees })
            "Angles are treated as degrees",
 Option ['r'] ["radians"] (NoArg $ \m -> m { angularMode = Radians })
            "Angles are treated as radians (default)",
 Option ['F'] ["floating"] (NoArg $ \m -> m { exactnessMode = Floating })
            "Floating exactness (default)",
 Option ['f'] ["fractional"] (NoArg $ \m -> m { exactnessMode = Fractional })
            "Fractional exactness (default)",
 Option ['s'] ["symbolic"] (NoArg $ \m -> m { exactnessMode = Symbolic })
            "Symbolic exactness (default)"
 ]

parseArgv :: [String] -> ModeInfo -> IO (ModeInfo, [String])
parseArgv argv mode =
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (foldr id mode o, n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ./Main [OPTION...]"

parseArgv' :: [String] -> IO (ModeInfo, [String])
parseArgv' argv = parseArgv argv defaultMode
