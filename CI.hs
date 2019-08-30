-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- CI script, compatible with all of Travis, Appveyor and Azure.

import Control.Monad
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra

main :: IO ()
main = do
  -- Linux only for now.
  cmd "stack build --no-terminal --interleaved-output"
  cmd "stack run -- test"
  where
    cmd :: String -> IO ()
    cmd x = do
      putStrLn $ "\n\n# Running: " ++ x
      hFlush stdout
      (t, _) <- duration $ system_ x
      putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
      hFlush stdout
