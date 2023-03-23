module MusicLib where

import Sound.PortMidi
import Control.Concurrent
import Data.List

-- Music interface

data MusObj = Note  Integer Integer Integer | 
              Chord Integer [MusObj] | 
              Measure [MusObj] deriving (Show) 

--Calcule la duree d'un objet musical
getOnset :: MusObj -> Integer
getOnset (Note p d v) = 0
getOnset (Chord onset elems) = onset
getOnset (Measure elems) = 0

getDur :: MusObj -> Integer
getDur (Note p d v) = d
getDur (Chord onset elems) = foldl max 0 (map getDur elems)
getDur (Measure elems) = foldl max 0 (map (\x -> (getDur x) + (getOnset x)) elems)

----
collectMidiNote :: Integer->Integer->Integer->Integer->[(Integer,PMMsg)]
collectMidiNote p d v at  =
 let noteOn = PMMsg 0x90 (fromIntegral p) (fromIntegral v)
     noteOff = PMMsg 0x90 (fromIntegral p) 0 in
 [(at, noteOn),(at + d, noteOff)]

collectMidi :: MusObj -> Integer -> [(Integer, PMMsg)]
collectMidi (Note p d v) at = collectMidiNote p d v at
--collectMidi (Chord onset elems) at = 
--collectMidi (Measure elems) at = 

myPredicate :: Ord a => (a, b1) -> (a, b2) -> Ordering
myPredicate (a1, a2) (b1, b2) = compare a1 b1

sortMidi ::  [(Integer,PMMsg)] -> [(Integer,PMMsg)]
sortMidi = sortBy myPredicate

play :: MusObj -> PMStream -> IO ()
play obj stream = do
  startTime <- time
  let dur = getDur obj
  let midiEvents = sortMidi (collectMidi obj 0)
  let evts = map (\(t,msg) -> PMEvent {message = encodeMsg msg, 
    timestamp = fromIntegral t + startTime}) midiEvents
  writeEvents stream evts
  threadDelay (fromIntegral (dur*1000))
  return ()