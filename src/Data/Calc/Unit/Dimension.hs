{-# LANGUAGE DataKinds, KindSignatures #-}

module Data.Calc.Unit.Dimension where

data Dimension = Angle | Length
                 deriving (Show, Read, Eq, Ord, Enum)

class KnownDim (d :: Dimension) where
    runDim :: proxy d -> Dimension

instance KnownDim 'Angle where
    runDim _ = Angle

instance KnownDim 'Length where
    runDim _ = Length
