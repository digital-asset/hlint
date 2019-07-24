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
  let extra_lib_dirs="`stack path --compiler-bin`/../lib/ghc-8.6.5/rts"
      set_path = "LD_LIBRARY_PATH=" ++ extra_lib_dirs ++ " "
  cmd $ set_path ++ "stack build --no-terminal --interleaved-output --extra-lib-dirs " ++ extra_lib_dirs
  cmd $ set_path ++ "stack exec -- hlint test"
  where
    cmd :: String -> IO ()
    cmd x = do
      putStrLn $ "\n\n# Running: " ++ x
      hFlush stdout
      (t, _) <- duration $ system_ x
      putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
      hFlush stdout
