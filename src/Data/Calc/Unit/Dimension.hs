{-# LANGUAGE DataKinds, KindSignatures #-}

module Data.Calc.Unit.Dimension where

data Dimension = Angle | Length | Time
                 deriving (Show, Read, Eq, Ord, Enum)

-- TODO I guess we don't really need the KnownDim magic anymore.

class KnownDim (d :: Dimension) where
    runDim :: proxy d -> Dimension

instance KnownDim 'Angle where
    runDim _ = Angle

instance KnownDim 'Length where
    runDim _ = Length
