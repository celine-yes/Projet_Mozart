{-# LINE 1 "Sound\\PortMidi\\DeviceInfo.hsc" #-}
{-# OPTIONS_HADDOCK hide #-}

module Sound.PortMidi.DeviceInfo (
    DeviceInfo(..)
  , peekDeviceInfo
  ) where

import Foreign
import Foreign.C



data DeviceInfo
  =  DeviceInfo
  { interface :: String
  , name      :: String
  , input     :: Bool
  , output    :: Bool
  , opened    :: Bool
  } deriving (Eq, Show)

peekDeviceInfo :: Ptr a -> IO DeviceInfo
peekDeviceInfo ptr = do
  s <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr >>= peekCString
{-# LINE 25 "Sound\\PortMidi\\DeviceInfo.hsc" #-}
  u <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr >>= peekCString
{-# LINE 26 "Sound\\PortMidi\\DeviceInfo.hsc" #-}
  i <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 27 "Sound\\PortMidi\\DeviceInfo.hsc" #-}
  o <- (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 28 "Sound\\PortMidi\\DeviceInfo.hsc" #-}
  d <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 29 "Sound\\PortMidi\\DeviceInfo.hsc" #-}
  return $ DeviceInfo s u (asBool i) (asBool o) (asBool d)
  where
    asBool :: Int32 -> Bool
{-# LINE 32 "Sound\\PortMidi\\DeviceInfo.hsc" #-}
    asBool = (/= 0)
