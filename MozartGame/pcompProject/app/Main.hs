module Main where

import Control.Concurrent
import Sound.PortMidi
import Midi
import Text.Read (readMaybe)

midiDevicePrint :: Int -> IO ()
midiDevicePrint 0 = getDeviceInfo 0 >>= print
midiDevicePrint n = do
  getDeviceInfo n >>= print
  midiDevicePrint (n - 1)

main :: IO ()
main = do
  putStrLn "Le Jeu de Mozart "
  initialize
  n <- countDevices
  midiDevicePrint (n - 1)
  putStrLn "Entrez le numéro du périphérique de sortie souhaité :"
  line <- getLine
  let maybeDeviceId = readMaybe line :: Maybe Int
  case maybeDeviceId of
    Nothing -> putStrLn "Entrée invalide, veuillez saisir un nombre."
    Just deviceId -> do
      result <- openOutput deviceId 1
      case result of
        Left err -> return ()
        Right stream -> do
          sendMidiNote 60 1000 100 0 stream
          threadDelay (2000 * 1000)
          _ <- close stream
          return ()
  terminate
  return ()

menu :: IO ()
menu = do
  putStrLn "Menu:"
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
      -- à compléter
    "2" -> do 
      stream <- getOutputDevice
      
      menu stream
    "3" -> do
    "7" -> do
      stream <- getOutputDevice
      close stream
    _ -> do
      putStrLn "Option invalide"
      menu


