module State where

import Control.Monad.State
import Sound.PortMidi 
import Control.Concurrent
import Sound.PortMidi


data GameState = GameState{
    midiDeviceId :: Int,
    midiStream :: PMStream,
    instrumentId :: Int,
    transpositionId :: Int,
    miroirId :: Int,
    facteurId :: Float
}


-- Fonctions pour modifier l'etat du jeu en vérifiant qu'il
-- respecte bien les contraintes

setMidiDeviceId :: Int -> State GameState Int
setMidiDeviceId n = do
  id <- get
  put $ id {midiDeviceId = n} 
  return (n)

setMidiStream :: PMStream -> State GameState PMStream
setMidiStream stream = do
  s <- get
  put $ s {midiStream = stream}
  return (stream)


setInstrumentId :: Int -> State GameState Int
setInstrumentId n = do
  if n >= 0 && n <= 128 then do
    id <- get
    put $ id {instrumentId = n} 
    return (n)
  else return (-1)

setTranspositionId :: Int -> State GameState Int
setTranspositionId n = do
  if n >= 0 && n <= 2 then do
    id <- get
    put $ id {transpositionId = n} 
    return (n)
  else return (-1)


setMiroirId :: Int -> State GameState Int
setMiroirId n = do
  if n >= 0 && n <= 1 then do
    id <- get
    put $ id {miroirId = n} 
    return (n)
  else return (-1)

setFacteurId :: Float -> State GameState Float
setFacteurId n = do
  if n > 0.0 then do
    id <- get
    put $ id {facteurId = n} 
    return (n)
  else return (-1.0)

-- Fonctions pour récupérer l'etat du jeu

getMidiDeviceId :: State GameState Int
getMidiDeviceId = do
  id <- get
  return (midiDeviceId id)

getMidiStream :: State GameState PMStream
getMidiStream = do
  s <- get
  return (midiStream s)

getInstrumentId :: State GameState Int
getInstrumentId = do
  id <- get
  return (instrumentId id)

getTranspositionId :: State GameState Int
getTranspositionId = do
  id <- get 
  return (transpositionId id)

getMiroirId :: State GameState Int
getMiroirId = do
  id <- get 
  return (miroirId id)

getFacteurId :: State GameState Float
getFacteurId = do
  id <- get 
  return (facteurId id)


