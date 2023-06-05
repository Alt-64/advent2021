{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_advent (
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/jish/Code/advent_of_code/2022-haskell/.stack-work/install/x86_64-linux-tinfo6/47ed71d95514950860994122ce13724b5549bbc88f9f1db45c50fdb5d9ca3f9b/9.4.5/bin"
libdir     = "/home/jish/Code/advent_of_code/2022-haskell/.stack-work/install/x86_64-linux-tinfo6/47ed71d95514950860994122ce13724b5549bbc88f9f1db45c50fdb5d9ca3f9b/9.4.5/lib/x86_64-linux-ghc-9.4.5/advent-0.0.0-BrSCzEt5zl3CQ3frNNi4jY-day4"
dynlibdir  = "/home/jish/Code/advent_of_code/2022-haskell/.stack-work/install/x86_64-linux-tinfo6/47ed71d95514950860994122ce13724b5549bbc88f9f1db45c50fdb5d9ca3f9b/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/jish/Code/advent_of_code/2022-haskell/.stack-work/install/x86_64-linux-tinfo6/47ed71d95514950860994122ce13724b5549bbc88f9f1db45c50fdb5d9ca3f9b/9.4.5/share/x86_64-linux-ghc-9.4.5/advent-0.0.0"
libexecdir = "/home/jish/Code/advent_of_code/2022-haskell/.stack-work/install/x86_64-linux-tinfo6/47ed71d95514950860994122ce13724b5549bbc88f9f1db45c50fdb5d9ca3f9b/9.4.5/libexec/x86_64-linux-ghc-9.4.5/advent-0.0.0"
sysconfdir = "/home/jish/Code/advent_of_code/2022-haskell/.stack-work/install/x86_64-linux-tinfo6/47ed71d95514950860994122ce13724b5549bbc88f9f1db45c50fdb5d9ca3f9b/9.4.5/etc"

getBinDir     = catchIO (getEnv "advent_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "advent_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "advent_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "advent_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "advent_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "advent_sysconfdir") (\_ -> return sysconfdir)




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
