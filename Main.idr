module Main

import Violet

main : IO ()
main = do
  -- ignore the first argument, it's command itself
  (_ :: args) <- getArgs
  -- putStrLn $ show args
  run $ handle args
