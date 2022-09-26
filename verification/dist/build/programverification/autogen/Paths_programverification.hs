{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_programverification (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Trist\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Trist\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3\\programverification-0.1.0.0-22YeTDMdnpu3bQWAHTHJLS-programverification"
dynlibdir  = "C:\\Users\\Trist\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3"
datadir    = "C:\\Users\\Trist\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.3\\programverification-0.1.0.0"
libexecdir = "C:\\Users\\Trist\\AppData\\Roaming\\cabal\\programverification-0.1.0.0-22YeTDMdnpu3bQWAHTHJLS-programverification\\x86_64-windows-ghc-8.6.3\\programverification-0.1.0.0"
sysconfdir = "C:\\Users\\Trist\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "programverification_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "programverification_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "programverification_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "programverification_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "programverification_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "programverification_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
