module Csound.Sam.Trig (
	-- * Char sampler
	samCharTrig, samCharTap, samCharPush, samCharGroup, samCharCycle,

	-- * Midi sampler
	samMidiTrig, samMidiTap, samMidiPush, samMidiGroup,

	-- ** Generic functions
	samMidiTrigBy, samMidiTapBy, samMidiPushBy, samMidiGroupBy,
) where

import Data.Foldable(Foldable(foldMap))
import Data.Traversable hiding (mapM)
import Control.Arrow(first, second)

import Csound.Base
import qualified Csound.Sam.Core as S
import Csound.Sam.Core(Sam, bindSam)

------------------------------------------------------

-- | Triggers the sample with any char from the first string
-- and stops the sample with any char from the second string.
samCharTrig :: String -> String -> Sam -> Sam
samCharTrig starts stops = fmap (charTrig starts stops) 

-- | Plays a sample while the key is pressed.
samCharPush :: Char -> Sam -> Sam
samCharPush ch = fmap (charPush ch)

-- | Toggles the sample when the key is pressed.
samCharToggle :: Char -> Sam -> Sam
samCharToggle ch = fmap (charToggle ch)

-- | Char trigger with fixed note limiting by length in second.
-- It's useful optimization. It's good to use for drum notes and short sounds.
samCharTap :: D -> String -> Sam -> Sam
samCharTap stop starts = fmap (charTap stop starts)

-- | Plays one of the sample from the list when corresponding char is pressed.
-- The last string is for stopping the samples.
samCharGroup :: [(Char, Sam)] -> String -> Sam
samCharGroup as stop = fmap (\xs -> charGroup (zip starts xs) stop) $ sequenceA sams
	where (starts, sams) = unzip as

-- | Plays samples in sequence when key is pressed. The last string is 
-- for stopping the sequence.
samCharCycle :: Char -> String -> [Sam] -> Sam
samCharCycle start stop as = fmap (charCycle start stop) (sequenceA as)

------------------------------------------------------

-- | Triggers a sample with midi key.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
samMidiTrig :: MidiChn -> Int -> Sam -> Sam
samMidiTrig = samMidiTrigBy midiAmpInstr

-- | Midi trigger with fixed note limiting by length in second.
-- It's useful optimization. It's good to use for drum notes and short sounds.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
samMidiTap :: MidiChn -> D -> Int -> Sam -> Sam
samMidiTap = samMidiTapBy midiAmpInstr

samMidiPush :: MidiChn -> Int -> Sam -> Sam
samMidiPush = samMidiPushBy midiAmpInstr

-- | Toggles samples with midi key.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
samMidiToggle :: MidiChn -> Int -> Sam -> Sam
samMidiToggle = samMidiToggleBy midiAmpInstr

-- | Plays samples in the group. It's like the samCharGroup.
-- The key is an integer midi code. The C1 is 60 and the A1 is 69.
samMidiGroup :: MidiChn -> [(Int, Sam)] -> Sam
samMidiGroup midiChn as = S.liftSam $ fmap (\xs -> midiGroup midiChn $ zip keys xs) $ sequenceA sams
	where (keys, sams) = unzip as

-- | Generic samMidiTrig. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiTrigBy :: MidiTrigFun Sig2 -> MidiChn -> Int -> Sam -> Sam
samMidiTrigBy midiFun midiChn key = bindSam (midiTrigBy midiFun midiChn key)

-- | Generic samMidiTap. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiTapBy :: MidiTrigFun Sig2 -> MidiChn -> D -> Int -> Sam -> Sam
samMidiTapBy midiFun midiChn dt key = bindSam (midiTapBy midiFun midiChn dt key)

-- | Generic samMidiPush. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiPushBy :: MidiTrigFun Sig2 -> MidiChn -> Int -> Sam -> Sam
samMidiPushBy midiFun midiChn key = bindSam (midiPushBy midiFun midiChn key)

-- | Generic samMidiToggle. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiToggleBy :: MidiTrigFun Sig2 -> MidiChn -> Int -> Sam -> Sam
samMidiToggleBy midiFun midiChn key = bindSam (midiToggleBy midiFun midiChn key)

-- | Generic samMidiGroup. We can specify the midi triggering function.
-- The midi function takes in a signal and a volume of the pressed key (it ranges from 0 to 1).
-- It produces some output. The default is scaling the signal with the amplitude.
samMidiGroupBy :: MidiTrigFun Sig2 -> MidiChn -> [(Int, Sam)] -> Sam
samMidiGroupBy  midiFun midiChn as = S.liftSam $ fmap (\xs -> midiGroupBy midiFun midiChn $ zip keys xs) $ sequenceA sams
	where (keys, sams) = unzip as