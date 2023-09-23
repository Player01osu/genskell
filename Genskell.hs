module Genskell ( checkIncremental
                , globFiles
                , getModificationTime
                , incrementalExecute
                , parseSubCommands
                , quoteString
                , stdoutCmd
                , tcallProcess
                , callProcess
                , tcreateDirectoryIfMissing
                , trace
                , tremovePathForcibly
                , tstdoutCmd
                , addExtension
                , joinPath
                , takeBaseName
                , fileExist ) where

import System.Process ( callProcess
                      , readProcess )

import System.Directory ( createDirectoryIfMissing
                        , removePathForcibly )

import System.FilePath.Posix ( FilePath )
import Text.Printf ( printf )
import System.Environment ( getArgs )
import Control.Monad ( liftM2 )
import System.Posix.Files ( modificationTime
                          , getFileStatus
                          , fileExist )
import System.FilePath.Posix ( addExtension
                             , joinPath
                             , takeBaseName )

if' :: Bool -> a -> a -> a
if' p a b = if p then a else b

bool :: a -> a -> Bool -> a
bool b a p = if' p a b

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p a b = p >>= bool b a

checkIncremental (src, out) =
  ifM (fileExist out)
      (let srcTime = getModificationTime src
           outTime = getModificationTime out
      in liftM2 (>) srcTime outTime)
      (return True)

globFiles path pattern = stdoutCmd "find" [path, "-name", pattern] ""
getModificationTime path = (getFileStatus path) >>= return . modificationTime

quoteString = printf "'%s'"

stdoutCmd :: FilePath -> [String] -> String -> IO [String]
stdoutCmd cmd args stdin = (readProcess cmd args stdin) >>= return . lines
tstdoutCmd cmd args stdin = trace cmd args >> stdoutCmd cmd args stdin

tcallProcess cmd args = trace cmd args >> callProcess cmd args

tcreateDirectoryIfMissing createParent path =
  (putStrLn $ printf "Creating: %s" path) >> createDirectoryIfMissing createParent path

tremovePathForcibly path =
  (putStrLn $ printf "Removing: %s" path) >> removePathForcibly path

trace cmd args = putStrLn $ printf "%s %s" cmd $ unwords $ map quoteString args

type Incremental = (FilePath, FilePath)

incrementalExecute incremental cmd args =
  ifM (checkIncremental incremental)
      (tcallProcess cmd args >> return True)
      (return False)

type SubCommands = [([String], IO ())]

parseSubCommands subCommands defaultSubCommand =
  getArgs >>= go subCommands defaultSubCommand
  where
    go _ defaultSubCommand [] = defaultSubCommand
    go subCommands _ (x:xs)   = (runSubCommand subCommands x) >> go subCommands (return ()) xs

    runSubCommand [] arg = putStrLn $ printf "Invalid Subcommand: %s" arg
    runSubCommand ((patterns, action):xs) arg
      | elem arg patterns = action
      | otherwise         = runSubCommand xs arg
