{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_GCLparser (
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
version = Version [0,2,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/jessefloren/.cabal/bin"
libdir     = "/Users/jessefloren/.cabal/lib/x86_64-osx-ghc-9.2.4/GCLparser-0.2.1-inplace"
dynlibdir  = "/Users/jessefloren/.cabal/lib/x86_64-osx-ghc-9.2.4"
datadir    = "/Users/jessefloren/.cabal/share/x86_64-osx-ghc-9.2.4/GCLparser-0.2.1"
libexecdir = "/Users/jessefloren/.cabal/libexec/x86_64-osx-ghc-9.2.4/GCLparser-0.2.1"
sysconfdir = "/Users/jessefloren/.cabal/etc"

getBinDir     = catchIO (getEnv "GCLparser_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "GCLparser_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "GCLparser_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "GCLparser_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GCLparser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GCLparser_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
