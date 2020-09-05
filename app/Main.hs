{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Lang.Eval
import           Lang.Infer
import           Lang.Parser
import           Lang.Pretty
import           Lang.Syntax
import qualified Lang.TypeEnv               as TypeEnv

import qualified Data.Map                   as Map
import           Data.Monoid
import qualified Data.Text.Lazy             as L
import qualified Data.Text.Lazy.IO          as L

import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           Data.List                  (foldl', isPrefixOf)

import           System.Console.Repline
import           System.Environment
import           System.Exit


-- Okay so right now I want only evaluate entered expressions and manage state in Repl monad
-- I Have included types but they're used in any way


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data IState = IState
  { typeEnv :: TypeEnv-- Type environment
  , termEnv :: TermEnv  -- Value environment
  } deriving (Show)

initState :: IState
initState = IState Map.empty Map.empty

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-- This does not work yet!
-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env name expr =
  let v = evaluate env expr
      env' = Map.insert name v env'
    in (v, env')

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx'
  where (_, tmctx') = runEval env nm ex

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  -- Get the current interpreter state
  st <- get

  -- Parser ( returns AST )
  mod <- hoistErr $ parseModule "<stdin>" source

  -- Type Inference ( returns Typing Environment )
  -- tyctx' <- hoistErr $ inferTop (tyctx st) mod
  let tmctx = termEnv st

  -- Create the new environment
  let st' = st { termEnv = foldl' evalDef tmctx mod
               , typeEnv = Map.empty
               }

  -- Update the interpreter state
  when update (put st')

  -- If a value is entered, print it.
  liftIO $ putStrLn $ show st'
  case lookup "it" mod of
    Nothing -> return ()
    Just ex -> do
      let (val, st'') = runEval (termEnv st') "it"  ex
      liftIO $ putStrLn $ show val
      showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st = do
  case TypeEnv.lookup (typeEnv st)  "it" of
    Just val -> liftIO $ putStrLn $ show (arg, val)
    Nothing  -> return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: String -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ [show (typeEnv st)]

-- :load command
load :: String -> Repl ()
load args = do
  contents <- liftIO $ L.readFile args
  exec True contents

-- :type command
typeof :: String -> Repl ()
typeof arg = do
  st <- get
  let ctx = typeEnv st
  case TypeEnv.lookup ctx arg of
    Just val -> liftIO $ putStrLn $ show (arg, val)
    Nothing  -> exec False (L.pack arg)

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  -- , (":type"  , values)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  ctx <- gets termEnv
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

shellOptions :: [(String, String -> Repl ())]
shellOptions = [
    ("load"   , load)
  , ("browse" , browse)
  , ("quit"   , quit)
  , ("type"   , Main.typeof)
  ]


customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure ">>> "
customBanner MultiLine  = pure "| "

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState
     $ evalRepl customBanner cmd shellOptions Nothing (pure "FooLang> ")  completer pre final

-------------------------------------------------------------------------------
-- Topleve
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []              -> shell (return ())
    [fname]         -> shell (load fname)
    ["test", fname] -> shell (load fname >> browse [] >> quit ())
    _               -> putStrLn "invalid arguments"

