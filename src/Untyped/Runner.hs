module Untyped.Runner where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import Untyped.Eval
import Untyped.Parser
import Untyped.Syntax

showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn ((replicate d ' ') ++ "=> " ++ (show x))

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let (out, steps) = runEval ex
      mapM_ showStep steps
      print . show $ out

run :: IO ()
run = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Untyped> "
      case minput of
        Nothing -> outputStrLn "Goodbye. "
        Just input -> (liftIO $ process input) >> loop
