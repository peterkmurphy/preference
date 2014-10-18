module Paths_preference (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/peter/.cabal/bin"
libdir     = "/home/peter/.cabal/lib/preference-0.1.0/ghc-7.6.3"
datadir    = "/home/peter/.cabal/share/preference-0.1.0"
libexecdir = "/home/peter/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "preference_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "preference_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "preference_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "preference_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
