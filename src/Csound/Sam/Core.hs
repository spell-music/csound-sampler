-- | The core types/ They are not imported by default.
{-# Language DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Csound.Sam.Core (
	Sample(..), S(..), Dur(..), Bpm
) where

import Control.Applicative
import Control.Monad.Trans.Reader

import Csound.Base

data Dur = Dur D | InfDur

-- | The Beats Per Minute measure (BPM). Almost all values are measured in BPMs.
type Bpm = D

-- | The generic type for samples.
newtype Sample a = Sam { unSam :: ReaderT Bpm SE (S a) 
	} deriving (Functor)

instance Applicative Sample where
	pure = Sam . pure . pure
	(Sam rf) <*> (Sam ra) = Sam $ liftA2 (<*>) rf ra

data S a = S
	{ samSig :: a
	, samDur :: Dur 
	} deriving (Functor)

instance Applicative S where
	pure a = S a InfDur
	(S f df) <*> (S a da) = S (f a) $ case (df, da) of
		(Dur durF, Dur durA) -> Dur $ maxB durF durA
		_			     -> InfDur

instance Num a => Num (Sample a) where
	(+) = liftA2 (+)
	(*) = liftA2 (*)
	(-) = liftA2 (-)
	negate = fmap negate
	abs = fmap abs
	signum = fmap signum
	fromInteger = pure . fromInteger

instance Fractional a => Fractional (Sample a) where
	recip = fmap recip
	fromRational = pure . fromRational

instance SigSpace a => SigSpace (Sample a) where
	mapSig f = fmap (mapSig f)

