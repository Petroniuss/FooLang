{-# LANGUAGE MultiParamTypeClasses #-}

module Lambda.Typed.Runner where

import           Control.Monad
import           Control.Monad.Trans
import           Lambda.Typed.Eval
import           Lambda.Typed.Parser
import           Lambda.Typed.Syntax
import           System.Console.Haskeline

import           Lambda.Typed.TypeChecker

instance Show TypeError where
  show (NotInScope arg ) = "Not in scope " <> arg
  show (TypeMismatch exp act) = "Couldn't match expected type: " <> (show exp) <> " with " <> (show act)
  show (NotFunction exp) = "Tried to apply to non-function type: " <> (show exp)


process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let eith = runCheck ex
      case eith of
        Left err -> do
          print . show $ err
        Right t -> do
          let res = runEval ex
          print $ ((show res) <> " :: " <> (show t))

run :: IO ()
run = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "TypedLambda> "
      case minput of
        Nothing    -> outputStrLn "Goodbye. "
        Just input -> (liftIO $ process input) >> loop
