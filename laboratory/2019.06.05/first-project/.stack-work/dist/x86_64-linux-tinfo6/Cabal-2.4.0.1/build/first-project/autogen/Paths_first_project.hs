{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_first_project (
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

bindir     = "/home/alessandro/Code/UFABC/2019.2/PP-2019.2/laboratory/2019.06.05/first-project/.stack-work/install/x86_64-linux-tinfo6/lts-13.24/8.6.5/bin"
libdir     = "/home/alessandro/Code/UFABC/2019.2/PP-2019.2/laboratory/2019.06.05/first-project/.stack-work/install/x86_64-linux-tinfo6/lts-13.24/8.6.5/lib/x86_64-linux-ghc-8.6.5/first-project-0.1.0.0-LIBXSPy8fyA5fXRYIw1n0U-first-project"
dynlibdir  = "/home/alessandro/Code/UFABC/2019.2/PP-2019.2/laboratory/2019.06.05/first-project/.stack-work/install/x86_64-linux-tinfo6/lts-13.24/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/alessandro/Code/UFABC/2019.2/PP-2019.2/laboratory/2019.06.05/first-project/.stack-work/install/x86_64-linux-tinfo6/lts-13.24/8.6.5/share/x86_64-linux-ghc-8.6.5/first-project-0.1.0.0"
libexecdir = "/home/alessandro/Code/UFABC/2019.2/PP-2019.2/laboratory/2019.06.05/first-project/.stack-work/install/x86_64-linux-tinfo6/lts-13.24/8.6.5/libexec/x86_64-linux-ghc-8.6.5/first-project-0.1.0.0"
sysconfdir = "/home/alessandro/Code/UFABC/2019.2/PP-2019.2/laboratory/2019.06.05/first-project/.stack-work/install/x86_64-linux-tinfo6/lts-13.24/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "first_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "first_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "first_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "first_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "first_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "first_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
