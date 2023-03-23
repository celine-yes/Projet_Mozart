{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_PortMidi (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,2,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "D:\\S6\\MozartGame\\.stack-work\\install\\2bb75d01\\bin"
libdir     = "D:\\S6\\MozartGame\\.stack-work\\install\\2bb75d01\\lib\\x86_64-windows-ghc-9.2.7\\PortMidi-0.2.0.0-GbWO5j5hLDw6QbQ0nFVlvE"
dynlibdir  = "D:\\S6\\MozartGame\\.stack-work\\install\\2bb75d01\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "D:\\S6\\MozartGame\\.stack-work\\install\\2bb75d01\\share\\x86_64-windows-ghc-9.2.7\\PortMidi-0.2.0.0"
libexecdir = "D:\\S6\\MozartGame\\.stack-work\\install\\2bb75d01\\libexec\\x86_64-windows-ghc-9.2.7\\PortMidi-0.2.0.0"
sysconfdir = "D:\\S6\\MozartGame\\.stack-work\\install\\2bb75d01\\etc"

getBinDir     = catchIO (getEnv "PortMidi_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "PortMidi_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "PortMidi_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "PortMidi_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PortMidi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PortMidi_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
