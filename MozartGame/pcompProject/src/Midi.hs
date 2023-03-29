module Midi where
import MusicLib
import Sound.PortMidi
import Control.Concurrent (forkIO, threadDelay)
import DataBase
import System.Random (randomRIO)
import State
import Control.Monad.State


playAfterDelay :: Int -> IO (Either PMError PMSuccess) ->  IO ()
playAfterDelay t f = forkIO (threadDelay t >> f >> return ()) >> return ()

sendMidiNote :: Integer->Integer->Integer->Integer->PMStream->IO ()
sendMidiNote p d v at stream = do
 startTime <- time
 let noteOn = PMMsg 0x90 (fromIntegral $ p) (fromIntegral $ v)
     evt1   = PMEvent (encodeMsg noteOn) startTime
     noteOff = PMMsg 0x90 (fromIntegral $ p) (fromIntegral $ 0)
     evt2   = PMEvent (encodeMsg noteOff) startTime
 playAfterDelay (fromIntegral $ (at * 1000)) (writeShort stream evt1)
 playAfterDelay (fromIntegral $ ((at + d) * 1000)) (writeShort stream evt2)

changeInstrument :: Integer->PMStream->IO ()
changeInstrument num stream = do
 startTime <- time
 let pgmchange = PMMsg 0x1C (fromIntegral $ num) (fromIntegral $ 0)
     evt1   = PMEvent (encodeMsg pgmchange) startTime
 writeShort stream evt1
 return ()

noteCount :: MusObj -> Integer
noteCount (Note _ _ _) = 1
noteCount (Chord _ liste) = sum (map noteCount liste)
noteCount (Measure liste) = sum (map noteCount liste)

-- augmente ou réduit la durée d'un objet musical
stretch :: MusObj -> Float -> MusObj
stretch (Note h d v) factor = Note h (round (fromIntegral d * factor)) v
stretch (Chord date l) factor = Chord date (map (\e -> stretch e factor) l)
stretch (Measure l) factor = Measure (map (\e -> stretch e factor) l)

-- augmente la hauteur d'un objet musical 
transpose :: MusObj -> Integer -> MusObj
transpose (Note h d v) n = Note (h+n) d v
transpose (Chord date l) n = Chord date (map (\e -> transpose e n) l)
transpose (Measure l) n = Measure (map (\e -> transpose e n) l)


mirror :: MusObj -> Integer -> MusObj
mirror (Note h d v) c = Note (c - (h - c)) d v
mirror (Chord date l) c = Chord date (map (\e -> mirror e c) l)
mirror (Measure l) c = Measure (map (\e -> mirror e c) l)



{- 
--------------------------------------------------------
Implémentation des fonctions permettant de simuler le jeux
--------------------------------------------------------
-}

-- fonction qui lance deux dés et renvoie la somme des deux dés
rollDice :: IO Int
rollDice = do
  dice1 <- randomRIO (1, 6) :: IO Int
  dice2 <- randomRIO (1, 6) :: IO Int
  return (dice1 + dice2)

-- fonction qui renvoie l'objet musical correspondant au résultat de rollDice
getMeasure :: Int -> Int -> [[Int]] -> IO MusObj
getMeasure diceResult measureNumber partie = do
  -- return (measures !! 0) -- Test
  let mn = measureNumber - 1  -- indice colonne
  let dr = diceResult - 2     -- indice ligne
  let numberMusObj = (partie !! dr) !! mn
  -- putStrLn $ "measures[" ++ show numberMusObj ++ "]"
  return (measures !! numberMusObj)


-- Fonction appliquant les configurations sur une mesure
applyTransformations :: GameState -> MusObj -> IO MusObj
applyTransformations state measure = do
  let (transposition, _) = runState getTranspositionId state
  let (miroir, _) = runState getMiroirId state
  let (facteur, _) = runState getFacteurId state

  -- transpose l'OM
  -- putStrLn $ "transposition = " ++ show transposition
  let transposed = if transposition /= 0
            then if transposition == 1
              then transpose measure 12
              else transpose measure (-12)
            else measure

  -- modifie la durée de l'OM
  let measure = stretch transposed facteur

  -- bascule mode miroir si miroir = 1
  let mirored = if miroir /= 0
            then mirror measure 60
            else measure

  return mirored


  
-- fonction permettant de jouer huit mesures
perform8Measures :: GameState -> PMStream -> [[Int]] -> Int -> IO ()
perform8Measures state stream partie iterator= do
  {---------------------------------------------------test
  measure <- getMeasure 9 iterator partie
  transformedMeasure <- applyTransformations state measure
  play transformedMeasure stream
  --------------------------------------------------test-}
  if iterator == 9 then
    return ()
  else do
    measureNumber <- rollDice  -- jeu de dés
    -- récupère la mesure en fonction du résultat de rollDice
    measure <- getMeasure measureNumber iterator partie
    -- applique les différentes configurations sur la mesure
    transformedMeasure <- applyTransformations state measure
    -- joue la mesure bien configurée
    play transformedMeasure stream

    perform8Measures state stream partie (iterator+1)


-- Fonction permettant de jouer un menuet 
playMenuet :: GameState -> IO ()
playMenuet state = do
  -- ouvre la sortie MIDI
  let (midiId, _) = runState getMidiDeviceId state
  result <- openMidi midiId
  let stream = either (error . show) id result
  -- modifie l'instrument
  let (instrument, _) = runState getInstrumentId state
  changeInstrument (fromIntegral instrument) stream
  -- 1ere partie correspondant au tableau d'indice part1
  perform8Measures state stream part1 1
  -- 2eme partie correspondant au tableau d'indice part2
  perform8Measures state stream part2 1

  void $ close stream


-- fonction qui affiche l'état du jeu
printState :: GameState -> IO ()
printState state = do
  let (midi, _) = runState getMidiDeviceId state
  let (instrument, _) = runState getInstrumentId state
  let (transposition, _) = runState getTranspositionId state
  let (miroir, _) = runState getMiroirId state
  let (facteur, _) = runState getFacteurId state

  putStrLn $ "\nConfiguration: "
  putStrLn $ "Instrument: " ++ show instrument
  putStrLn $ "Transposition: " ++ show transposition
  putStrLn $ "Miroir: " ++ show miroir
  putStrLn $ "Durée: x" ++ show facteur
  putStrLn $ "Sortie Midi: " ++ show midi
  putStrLn $ ""

--Fontion qui renvoie le stream du port MIDI ouvert supposant qu'il est valide
openMidi :: Int -> IO (Either PMError PMStream)
openMidi deviceId = do
  result <- openOutput deviceId 1
  case result of
    Left err -> do
      putStrLn $ "Erreur lors de l'ouverture du périphérique MIDI : " ++ show err
      return $ Left err
    Right stream -> return $ Right stream