module Lambda.Typed.Runner where

import Control.Monad
import Control.Monad.Trans
import Lambda.Typed.Eval
import Lambda.Typed.Parser
import Lambda.Typed.Syntax
import System.Console.Haskeline

showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn ((replicate d ' ') ++ "=> " ++ (show x))

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let res = runEval ex
      print . show $ res

run :: IO ()
run = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Untyped> "
      case minput of
        Nothing -> outputStrLn "Goodbye. "
        Just input -> (liftIO $ process input) >> loop
