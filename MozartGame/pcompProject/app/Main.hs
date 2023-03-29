module Main where

import Control.Concurrent
import Sound.PortMidi
import Midi
import Text.Read (readMaybe)
import State
import Control.Monad.State
import Io



main :: IO ()
main = do
  putStrLn "\nLe Jeu de Mozart\n"
  initialize

  let state = GameState { midiDeviceId = 0,
                          midiStream = undefined,
                          instrumentId = 1,
                          transpositionId = 0,
                          miroirId = 0,
                          facteurId = 1.0 }

  (stream, newState) <- chooseMidi state

  menu newState

  terminate
  return ()

menu :: GameState -> IO ()
menu s = do
  putStrLn "\nVeuillez entrer le numéro d'une instruction:\n"
  putStrLn "1. Jouer un morceau"
  putStrLn "2. Changer d'instrument"
  putStrLn "3. Transposer"
  putStrLn "4. Basculer le mode miroir"
  putStrLn "5. Changer la durée"
  putStrLn "6. Changer de sortie MIDI"
  putStrLn "7. Quitter"
  line <- getLine
  case line of
    "1" -> do
      printState s
      playMenuet s
      putStrLn "Fin du Menuet\n"
      menu s
    "2" -> do 
      newS <- demandeInstrument s
      printState newS
      menu newS
    "3" -> do
      newS <- demandeTransposition s
      printState newS
      menu newS
    "4" -> do
      newS <- demandeMiroir s  -- peut modifier miroir de sorte à le faire automatiquement en fonction de l'état actuel du miroir
      printState newS
      menu newS
    "5" -> do
      newS <- demandeFacteur s
      printState newS
      menu newS
    "6" -> do
      (_,newS) <- chooseMidi s
      printState newS
      menu newS
    "7" -> do
      putStrLn "\nAu revoir :D\n . \n . \n ."
    _ -> do
      putStrLn "Option invalide"
      menu s


