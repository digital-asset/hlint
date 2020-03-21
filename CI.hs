#!/usr/bin/env stack runhaskell --package extra --package optparse-applicative --
-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- CI script, compatible with all of Travis, Appveyor and Azure.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opts

main :: IO ()
main = do
   let opts =
         Opts.info (parseOptions Opts.<**> Opts.helper)
         ( Opts.fullDesc
           <> Opts.progDesc "Build and test hlint."
           <> Opts.header "CI - CI script for hlint"
         )
   Options {stackYaml} <- Opts.execParser opts
   process stackYaml

stackYamlOpt :: Maybe String -> String
stackYamlOpt = \case
  Just file -> "--stack-yaml " ++ file
  Nothing -> ""

data Options = Options
    { stackYaml :: Maybe String } deriving (Show)

parseOptions :: Opts.Parser Options
parseOptions = Options
    <$> Opts.optional ( Opts.strOption
        ( Opts.long "stack-yaml"
          <> Opts.help "If specified, the stack-yaml file to use"
        ))

process :: Maybe String -> IO ()
process config = do
  -- Feedback on the compiler used for building hlint.
  stack "exec -- ghc --version"
  stack "--no-terminal --interleaved-output build"
  stack "run -- test --with-refact=\"disable-refact-tests\""
  where
      stackYamlFlag :: String -- One of "" or, --stack-yaml=file.
      stackYamlFlag = stackYamlOpt config

      stack :: String -> IO ()
      stack action = cmd $ "stack " ++ stackYamlFlag ++ " " ++ action

      cmd :: String -> IO ()
      cmd x = do
        putStrLn $ "\n\n# Running: " ++ x
        hFlush stdout
        (t, _) <- duration $ system_ x
        putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
        hFlush stdout
