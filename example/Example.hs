#!/usr/bin/env runghc
import Control.Monad (foldM, liftM, liftM2, void)
import Genskell
  ( addExtension
  , globFiles
  , incrementalExecute
  , incrementalForEach
  , joinPath
  , parseSubCommands
  , takeBaseName
  , tcallProcess
  , tcreateDirectoryIfMissing
  , tremovePathForcibly
  )

buildDir = "build"

targetDir = "target"

bin = "main"

srcs :: IO [String]
objs :: IO [String]
srcsObjs :: IO [(String, String)]
srcs = globFiles "./" "*.c"

objs = srcs >>= return . map objName

srcsObjs = liftM2 zip srcs objs

objName src = joinPath [buildDir, addExtension (takeBaseName src) "o"]

compileObj :: (String, String) -> IO Bool
compileObj inc@(input, output) =
  let cmd = "clang"
      args = ["-c", "-o", output, input]
   in incrementalExecute inc cmd args

createBuildDir = tcreateDirectoryIfMissing True buildDir

createTargetDir = tcreateDirectoryIfMissing True targetDir

createObj = srcsObjs >>= incrementalForEach compileObj

linkObj =
  objs >>= \x -> tcallProcess "clang" $ ["-o", joinPath [targetDir, bin]] <> x

cleanBuildDir = tremovePathForcibly buildDir

cleanTargetDir = tremovePathForcibly targetDir

build = do
  createBuildDir
  createTargetDir
  recompiled <- createObj
  if recompiled
    then linkObj
    else putStrLn "Nothing to do"

clean = do
  cleanBuildDir
  cleanTargetDir

run = do
  build
  tcallProcess (joinPath [targetDir, bin]) []

defaultSubCommand = build

runSubCommands =
  parseSubCommands
    [(["b", "build"], build), (["c", "clean"], clean), (["r", "run"], run)]
    defaultSubCommand

main = runSubCommands
