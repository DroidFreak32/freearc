{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hslua (
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
version = Version [1,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/horcrux/.cabal/bin"
libdir     = "/home/horcrux/.cabal/lib/x86_64-linux-ghc-8.6.1/hslua-1.0.1-BmFMmUMk1Gy5siuRknzbpr"
dynlibdir  = "/home/horcrux/.cabal/lib/x86_64-linux-ghc-8.6.1"
datadir    = "/home/horcrux/.cabal/share/x86_64-linux-ghc-8.6.1/hslua-1.0.1"
libexecdir = "/home/horcrux/.cabal/libexec/x86_64-linux-ghc-8.6.1/hslua-1.0.1"
sysconfdir = "/home/horcrux/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hslua_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hslua_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hslua_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hslua_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hslua_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hslua_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
