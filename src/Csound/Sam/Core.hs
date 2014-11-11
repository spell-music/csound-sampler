-- | The core types/ They are not imported by default.
{-# Language DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Csound.Sam.Core (
	Sam, runSam, Sample(..), S(..), Dur(..), Bpm,
	liftSam, mapBpm, bindSam, bindBpm
) where

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Csound.Base

-- | The main type. A stereo sample.
type Sam = Sample Sig2

instance RenderCsd Sam where
    renderCsdBy opt sample = renderCsdBy opt (runSam (120 * 4) sample)

runSam :: Bpm -> Sam -> SE Sig2
runSam bpm x = fmap samSig $ runReaderT (unSam x) bpm

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

-- Lifters

-- | Hides the effects inside sample.
liftSam :: Sample (SE a) -> Sample a
liftSam (Sam ra) = Sam $ do
	a <- ra
	lift $ fmap (\x -> a{ samSig = x}) $ samSig a

-- | Transforms the sample with BPM.
mapBpm :: (Bpm -> Sig2 -> Sig2) -> Sam -> Sam
mapBpm f (Sam ra) = Sam $ do
	bpm <- ask
	a <- ra
	return $ a { samSig = f bpm $ samSig a }

-- | Lifts bind on stereo signals to samples.
bindSam :: (Sig2 -> SE Sig2) -> Sam -> Sam
bindSam f = liftSam . fmap f

-- | Lifts bind on stereo signals to samples with BPM.
bindBpm :: (Bpm -> Sig2 -> SE Sig2) -> Sam -> Sam
bindBpm f (Sam ra) = Sam $ do
	bpm <- ask
	a <- ra
	lift $ fmap (\x -> a{ samSig = x}) $ f bpm $ samSig a
