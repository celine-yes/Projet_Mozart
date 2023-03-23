module State where

import Control.Monad.State
import Sound.PortMidi 
import Control.Concurrent
import Sound.PortMidi
import Midi

data GameState = GameState{
    midiDeviceId :: Int,
    outputDevice :: Maybe OutputDevice,
    instrumentId :: Int,
    transpositionId :: Int,
    demitonsId :: Int,
    miroirId :: Int,
    facteurId :: Double
}

--  crée une monade State avec l'état
-- de l'application stocké dans AppState, 
-- combiné avec la monade IO pour permettre 
-- l'exécution d'actions d'entrée/sortie.
type Game = StateT GameState IO

-- Fonctions pour modifier l'etat du jeu en vérifiant qu'il
-- respecte bien les contraintes

setMidiDeviceId :: Int -> Game ()
setMidiDeviceId n = do
  deviceCount <- liftIO countDevices
  if n >= 0 && n < deviceCount
    then modify (\s -> s { midiDeviceId = n })
    else return ()

setOutputDevice :: Maybe OutputDevice -> Game ()
setOutputDevice n = modify (\s -> s { outputDevice = n })

setInstrumentId :: Int -> Game ()
setInstrumentId n
  | n >= 1 && n <= 5 = modify (\s -> s { instrumentId = n })
  | otherwise = return ()

setTranspositionId :: Int -> Game ()
setTranspositionId n
  | n >= 0 && n <= 2 = modify (\s -> s { transpositionId = n })
  | otherwise = return ()

setDemitonsId :: Int -> Game ()
setDemitonsId n
  | n >= 0 && n <= 2 = modify (\s -> s { demitonsId = n })
  | otherwise = return ()

setMiroirId :: Int -> Game ()
setMiroirId n
  | n >= 0 && n <= 1 = modify (\s -> s { miroirId = n })
  | otherwise = return ()

setFacteurId :: Double -> Game ()
setFacteurId n
  | n > 0.0 = modify (\s -> s { facteurId = n })
  | otherwise = return ()

-- Fonctions pour récupérer l'etat du jeu

getMidiDeviceId :: Game Int
getMidiDeviceId = gets midiDeviceId

getOutputDevice :: Game Maybe OutputDevice
getOutputDevice = gets outputDevice

getInstrumentId :: Game Int
getInstrumentId = gets instrumentId

getTranspositionId :: Game Int
getTranspositionId = gets transpositionId

getDemitonsId :: Game Int
getDemitonsId = gets demitonsId

getMiroirId :: Game Int
getMiroirId = gets miroirId

getFacteurId :: Game Double
getFacteurId = gets facteurId