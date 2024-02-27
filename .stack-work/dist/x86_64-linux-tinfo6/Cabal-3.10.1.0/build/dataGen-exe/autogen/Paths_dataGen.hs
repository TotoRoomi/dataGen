{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_dataGen (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/toto/programmering/dataGen/.stack-work/install/x86_64-linux-tinfo6/baf332d1dfe70c943033834fa703cdc1e969881e33039fd6e65c9c531fb572bd/9.6.4/bin"
libdir     = "/home/toto/programmering/dataGen/.stack-work/install/x86_64-linux-tinfo6/baf332d1dfe70c943033834fa703cdc1e969881e33039fd6e65c9c531fb572bd/9.6.4/lib/x86_64-linux-ghc-9.6.4/dataGen-0.1.0.0-IqStnFDQ4GULOvEGtWGkko-dataGen-exe"
dynlibdir  = "/home/toto/programmering/dataGen/.stack-work/install/x86_64-linux-tinfo6/baf332d1dfe70c943033834fa703cdc1e969881e33039fd6e65c9c531fb572bd/9.6.4/lib/x86_64-linux-ghc-9.6.4"
datadir    = "/home/toto/programmering/dataGen/.stack-work/install/x86_64-linux-tinfo6/baf332d1dfe70c943033834fa703cdc1e969881e33039fd6e65c9c531fb572bd/9.6.4/share/x86_64-linux-ghc-9.6.4/dataGen-0.1.0.0"
libexecdir = "/home/toto/programmering/dataGen/.stack-work/install/x86_64-linux-tinfo6/baf332d1dfe70c943033834fa703cdc1e969881e33039fd6e65c9c531fb572bd/9.6.4/libexec/x86_64-linux-ghc-9.6.4/dataGen-0.1.0.0"
sysconfdir = "/home/toto/programmering/dataGen/.stack-work/install/x86_64-linux-tinfo6/baf332d1dfe70c943033834fa703cdc1e969881e33039fd6e65c9c531fb572bd/9.6.4/etc"

getBinDir     = catchIO (getEnv "dataGen_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "dataGen_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "dataGen_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "dataGen_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dataGen_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dataGen_sysconfdir") (\_ -> return sysconfdir)



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
