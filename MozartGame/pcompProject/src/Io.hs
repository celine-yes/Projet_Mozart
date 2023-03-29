module Io where
import Control.Concurrent
import Sound.PortMidi
import Midi
import Text.Read (readMaybe)
import State
import Control.Monad.State


midiDevicePrint :: Int -> IO ()
midiDevicePrint 0 = getDeviceInfo 0 >>= print
midiDevicePrint n = do
  getDeviceInfo n >>= print
  midiDevicePrint (n - 1)


chooseMidi :: GameState -> IO (PMStream, GameState)
chooseMidi state= do
  n <- countDevices
  putStrLn "\nListe de vos Ports MIDI :"
  midiDevicePrint (n - 1)
  putStrLn "\nEntrez le numéro du périphérique de sortie souhaité :\n"
  line <- getLine
  let maybeDeviceId = readMaybe line :: Maybe Int
  case maybeDeviceId of
    Nothing -> putStrLn "\nEntrée invalide, veuillez saisir un nombre.\n" >> chooseMidi state
    Just deviceId -> do
      result <- openOutput deviceId 1
      case result of
        Left err -> putStrLn (show err) >> chooseMidi state
        Right stream -> do
          -- teste si le son sort bien, à enlever à la fin des tests !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          sendMidiNote 60 1000 100 0 stream
          threadDelay (2000 * 1000)
          let (newId, newState) = runState (setMidiDeviceId deviceId) state
          let (newStream, newState') = runState (setMidiStream stream) newState
          close stream              -- fermeture du stream, on le ré-ouvre si on veut jouer une note
          return (newStream, newState')




demandeInstrument :: GameState -> IO GameState
demandeInstrument state = do
  putStrLn "\nEntrez le numéro de l'instrument souhaité (0-128):\n"
  line <- getLine
  let maybeInstId = readMaybe line :: Maybe Int
  case maybeInstId of
      Nothing -> putStrLn "\nEntrée invalide, veuillez saisir un nombre." >> demandeInstrument state
      Just instId -> do
        let (midi, _) = runState getMidiDeviceId state
        result <- openMidi midi
        let stream = either (error . show) id result
        
        -- teste si le son sort bien, à enlever à la fin des tests !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        changeInstrument (fromIntegral instId) stream
        sendMidiNote 60 1000 100 0 stream
        threadDelay (2000 * 1000)
        let (newId, newState) = runState (setInstrumentId instId) state
    
        close stream              -- fermeture du stream, on le ré-ouvre si on veut jouer une note
        return newState


demandeTransposition :: GameState -> IO GameState
demandeTransposition state = do
  putStrLn "\nEntrez le numéro de transposition :"
  putStrLn "0: Pas de transposition\n1: +12 demitons\n2: -12 demitons"
  line <- getLine
  let maybeTransId = readMaybe line :: Maybe Int
  case maybeTransId of
      Nothing -> putStrLn "\nEntrée invalide, veuillez saisir un nombre.\n" >> demandeTransposition state
      Just transId -> do
        let (newId, newState) = runState (setTranspositionId transId) state
        
        return newState

demandeMiroir :: GameState -> IO GameState
demandeMiroir state = do
  putStrLn "\nEntrez le numéro de miroir :"
  putStrLn " 0: off       1: on"
  line <- getLine
  let maybeMirId = readMaybe line :: Maybe Int
  case maybeMirId of
      Nothing -> putStrLn "\nEntrée invalide, veuillez saisir un nombre.\n" >> demandeMiroir state
      Just mirId -> do
        let (newId, newState) = runState (setMiroirId mirId) state
        
        return newState

demandeFacteur :: GameState -> IO GameState
demandeFacteur state = do
  putStrLn "\nEntrez le facteur :"
  line <- getLine
  let maybeFacId = readMaybe line :: Maybe Float
  case maybeFacId of
      Nothing -> putStrLn "\nEntrée invalide, veuillez saisir un nombre.\n" >> demandeFacteur state
      Just facId -> do
        let (newId, newState) = runState (setFacteurId facId) state
        
        return newState