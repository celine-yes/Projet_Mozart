module MusicLib where

import Sound.PortMidi
import Control.Concurrent
import Data.List

-- Music interface

data MusObj = Note  Integer Integer Integer | 
              Chord Integer [MusObj] | 
              Measure [MusObj] deriving (Show) 


--renvoie l'instant d'attaque d'un objet musical
getOnset :: MusObj -> Integer
getOnset (Note p d v) = 0
getOnset (Chord onset elems) = onset
getOnset (Measure elems) = 0

-- renvoie la durée d'un objet musical
getDur :: MusObj -> Integer
getDur (Note p d v) = d
getDur (Chord onset elems) = foldl max 0 (map getDur elems)
getDur (Measure elems) = foldl max 0 (map (\x -> (getDur x) + (getOnset x)) elems)

-- génère une paire d'événements MIDI (Note On et Note Off) 
collectMidiNote p d v at  =
 let noteOn = PMMsg 0x90 (fromIntegral p) (fromIntegral v)
     noteOff = PMMsg 0x90 (fromIntegral p) 0 in
 [(at, noteOn),(at + d, noteOff)]


-- fonction qui génère une liste d'événements MIDI pour chaque élément d'un objet musical
collectMidi :: MusObj -> Integer -> [(Integer, PMMsg)]
collectMidi (Note p d v) at = collectMidiNote p d v at
collectMidi (Chord onset elems) at = concatMap (\elem -> collectMidi elem (at + onset)) elems
collectMidi (Measure elems) at = concatMap (\elem -> collectMidi elem (at + getOnset elem)) elems

-- fonction de comparaison pour trier une liste de tuples en fonction de la première composante de chaque tuple
myPredicate :: Ord a => (a, b1) -> (a, b2) -> Ordering
myPredicate (a1, a2) (b1, b2) = compare a1 b1


-- fonction qui renvoie la liste trié par
sortMidi ::  [(Integer,PMMsg)] -> [(Integer,PMMsg)]
sortMidi = sortBy myPredicate


-- fonction qui joue un objet musical sur un port MIDI spécifié
play :: MusObj -> PMStream -> IO ()
play obj stream = do
  startTime <- time     --obtient le temps actuel
  let dur = getDur obj  -- obtient la durée de l'objet
  let midiEvents = sortMidi (collectMidi obj 0)     --trie la liste d'évènement MIDI dans l'ordre
  let evts = map (\(t,msg) -> PMEvent {message = encodeMsg msg, 
    timestamp = fromIntegral t + startTime}) midiEvents

  --displayPMMsgList midiEvents

  writeEvents stream evts 
  threadDelay (fromIntegral (dur*1000))
  return ()


displayPMMsgList :: [(Integer, PMMsg)] -> IO ()
displayPMMsgList msgList = do
    let formattedMsgList = map (\(i, msg) -> "Index: " ++ show i ++ ", Message: " ++ show msg) msgList
    mapM_ putStrLn formattedMsgList



