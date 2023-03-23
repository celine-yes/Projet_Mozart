module Midi where
import MusicLib
import Sound.PortMidi
import Control.Concurrent (forkIO, threadDelay)

--playAfterDelay :: Int -> IO (Either PMError PMSuccess) -> IO ()
playAfterDelay :: Int -> IO (Either PMError PMSuccess) ->  IO ()
playAfterDelay t f = forkIO (threadDelay t >> f >> return ()) >> return ()

--sendMidiNote :: Integer->Integer->Integer->Integer->PMStream->IO ThreadId
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
changeInstrument  num stream = do
 startTime <- time
 let pgmchange = PMMsg 0x1C (fromIntegral $ num) (fromIntegral $ 0)
     evt1   = PMEvent (encodeMsg pgmchange) startTime
 writeShort stream evt1
 return ()

noteCount :: MusObj -> Integer
noteCount (Note _ _ _) = 1
noteCount (Chord _ liste) = sum (map noteCount liste)
noteCount (Measure liste) = sum (map noteCount liste)

stretch :: MusObj -> Float -> MusObj
stretch (Note h d v) factor = Note h (round (fromIntegral d * factor)) v
stretch (Chord date l) factor = Chord date (map (\e -> stretch e factor) l)
stretch (Measure l) factor = Measure (map (\e -> stretch e factor) l)

transpose :: MusObj -> Integer -> MusObj
transpose (Note h d v) n = Note (h+n) d v
transpose (Chord date l) n = Chord date (map (\e -> transpose e n) l)
transpose (Measure l) n = Measure (map (\e -> transpose e n) l)

mirror :: MusObj -> Integer -> MusObj
mirror (Note h d v) c = Note (c - (h - c)) d v
mirror (Chord date l) c = Chord date (map (\e -> mirror e c) l)
mirror (Measure l) c = Measure (map (\e -> mirror e c) l)