module Genskell
  ( checkIncremental
  , globFiles
  , getModificationTime
  , incrementalExecute
  , incrementalForEach
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
  , fileExist
  ) where

import System.Process (callProcess, readProcess)

import System.Directory (createDirectoryIfMissing, removePathForcibly)

import Control.Monad (foldM, liftM2)
import System.Environment (getArgs)
import System.FilePath.Posix (FilePath)
import System.FilePath.Posix (addExtension, joinPath, takeBaseName)
import System.Posix.Files (fileExist, getFileStatus, modificationTime)
import Text.Printf (printf)

type SubCommands = [([String], IO ())]

type Incremental = (FilePath, FilePath)

if' :: Bool -> a -> a -> a
if' p a b =
  if p
    then a
    else b

bool :: a -> a -> Bool -> a
bool b a p = if' p a b

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p a b = p >>= bool b a

checkIncremental :: (FilePath, FilePath) -> IO Bool
checkIncremental (src, out) =
  ifM
    (fileExist out)
    (let srcTime = getModificationTime src
         outTime = getModificationTime out
      in liftM2 (>) srcTime outTime)
    (return True)

globFiles :: String -> String -> IO [String]
globFiles path pattern = stdoutCmd "find" [path, "-name", pattern] ""

getModificationTime :: FilePath -> IO System.Posix.Types.EpochTime
getModificationTime path = (getFileStatus path) >>= return . modificationTime

quoteString :: String
quoteString = printf "'%s'"

stdoutCmd :: FilePath -> [String] -> String -> IO [String]
stdoutCmd cmd args stdin = (readProcess cmd args stdin) >>= return . lines

tstdoutCmd :: FilePath -> [String] -> String -> IO [String]
tstdoutCmd cmd args stdin = trace cmd args >> stdoutCmd cmd args stdin

callProcess :: FilePath -> [String] -> IO ()
tcallProcess cmd args = trace cmd args >> callProcess cmd args

tcreateDirectoryIfMissing :: Bool -> FilePath -> IO ()
tcreateDirectoryIfMissing createParent path =
  (putStrLn $ printf "Creating: %s" path) >>
  createDirectoryIfMissing createParent path

tremovePathForcibly :: FilePath -> IO ()
tremovePathForcibly path =
  (putStrLn $ printf "Removing: %s" path) >> removePathForcibly path

trace :: Text.Printf.PrintfArg t => t -> [String] -> IO ()
trace cmd args = putStrLn $ printf "%s %s" cmd $ unwords $ map quoteString args

mor :: IO Bool -> IO Bool -> IO Bool
mor = liftM2 (||)

incrementalForEach :: (a -> IO Bool) -> [a] -> IO Bool
incrementalForEach f = foldM (\x y -> mor (return x) (f y)) False

incrementalExecute :: (FilePath, FilePath) -> FilePath -> [String] -> IO Bool
incrementalExecute incremental cmd args =
  ifM
    (checkIncremental incremental)
    (tcallProcess cmd args >> return True)
    (return False)

parseSubCommands :: Foldable t => [(t String, IO ())] -> IO () -> IO ()
parseSubCommands subCommands defaultSubCommand =
  getArgs >>= go subCommands defaultSubCommand
  where
    go _ defaultSubCommand [] = defaultSubCommand
    go subCommands _ (x:xs) =
      (runSubCommand subCommands x) >> go subCommands (return ()) xs
    runSubCommand [] arg = putStrLn $ printf "Invalid Subcommand: %s" arg
    runSubCommand ((patterns, action):xs) arg
      | elem arg patterns = action
      | otherwise = runSubCommand xs arg
