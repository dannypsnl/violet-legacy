module Main

import Effects
import Effect.StdIO
import Effect.State
import Violet

inc : Eff () [STATE Int]
inc = do
  i <- get
  put (i + 1)
  pure ()

hello : Int -> Eff () [STDIO]
hello i = putStrLn $ "Hello world! " ++ show i

main : IO ()
main = do
  let r = runPure $ do
    put 1
    inc
    inc
    get
  run (hello r)
