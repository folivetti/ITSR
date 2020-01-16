{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_it (
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

bindir     = "/home/olivetti/Projects/Interaction-Transformation/Haskell/it/.stack-work/install/x86_64-linux/752bda34c5d89e63365ee8a622427615127659d480e8ba853fb667343f3a681b/8.6.5/bin"
libdir     = "/home/olivetti/Projects/Interaction-Transformation/Haskell/it/.stack-work/install/x86_64-linux/752bda34c5d89e63365ee8a622427615127659d480e8ba853fb667343f3a681b/8.6.5/lib/x86_64-linux-ghc-8.6.5/it-0.1.0.0-DhZLimushoQE8IChC2WYdE-it"
dynlibdir  = "/home/olivetti/Projects/Interaction-Transformation/Haskell/it/.stack-work/install/x86_64-linux/752bda34c5d89e63365ee8a622427615127659d480e8ba853fb667343f3a681b/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/olivetti/Projects/Interaction-Transformation/Haskell/it/.stack-work/install/x86_64-linux/752bda34c5d89e63365ee8a622427615127659d480e8ba853fb667343f3a681b/8.6.5/share/x86_64-linux-ghc-8.6.5/it-0.1.0.0"
libexecdir = "/home/olivetti/Projects/Interaction-Transformation/Haskell/it/.stack-work/install/x86_64-linux/752bda34c5d89e63365ee8a622427615127659d480e8ba853fb667343f3a681b/8.6.5/libexec/x86_64-linux-ghc-8.6.5/it-0.1.0.0"
sysconfdir = "/home/olivetti/Projects/Interaction-Transformation/Haskell/it/.stack-work/install/x86_64-linux/752bda34c5d89e63365ee8a622427615127659d480e8ba853fb667343f3a681b/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "it_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "it_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "it_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "it_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "it_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "it_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
