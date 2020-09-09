{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Lang.Eval
import           Lang.Infer
import           Lang.Parser
import           Lang.Pretty
import           Lang.Syntax
import qualified Lang.TypeEnv                                                     as TypeEnv

import qualified Data.Map                                                         as Map
import           Data.Monoid
import qualified Data.Text.Lazy                                                   as L
import qualified Data.Text.Lazy.IO                                                as L

import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           Data.List
                                                                                   (foldl',
                                                                                   isPrefixOf)

import           System.Console.Haskeline
import           System.Console.Repline
import           System.Environment
import           System.Exit
-- import           System.Process             (callCommand)
import           Data.Text.Prettyprint.Doc
                                                                                   (Doc,
                                                                                   annotate,
                                                                                   line,
                                                                                   pretty,
                                                                                   (<+>))
import           Data.Text.Prettyprint.Doc.Render.Text
                                                                                   (putDoc)
import           Data.Text.Prettyprint.Doc.Render.Tutorials.TreeRenderingTutorial
                                                                                   (Color (Green),
                                                                                   bold,
                                                                                   color)
import           Prettyprinter.Render.Terminal
                                                                                   (AnsiStyle)
import           Text.Parsec.Error
                                                                                   (ParseError)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- We also need fresh name supply
data IState = IState
  { typeEnv :: TypeEnv.TypeEnv  -- Type environment
  , termEnv :: TermEnv  -- Value environment
  } deriving (Show)

initState :: IState
initState = IState
  { typeEnv = Map.empty
  , termEnv = emptyTermEnv }

definitions :: IState -> [String]
definitions IState { termEnv = tEnv } = Map.keys tEnv

type Repl a = HaskelineT (StateT IState IO) a

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

-- Everything happens here!
exec :: L.Text -> Repl ()
exec source = do
  -- Get the current interpreter state
  st@IState { typeEnv = tpEnv, termEnv = tmEnv } <- get

  -- Parser ( returns AST )
  (ast, it) <- handleError $ parseModule "<stdin>" source

  -- Type Inference ( returns Typing Environment )
  tpEnv' <- handleError $ inferModule tpEnv ast

  -- Create the new environment ()
  let st' = IState { termEnv = foldl' evalDef tmEnv ast
                   , typeEnv = tpEnv'
                   }
  -- Update environment
  put st'

  -- If a value is entered, print it.
  -- This guy needs refactoring!
  case it of
    Nothing -> return ()
    Just ex -> do
      -- Eval expr
      tp <- handleError $ inferIt tpEnv' ex
      -- This one cannot fail :)
      let val = evalExpr tmEnv ex

      -- Show evaluated value alongside its type
      replRender $ prettyIt val tp


replRender :: Doc AnsiStyle -> Repl ()
replRender doc = liftIO $ render doc

handleError :: Show a => Either a b -> Repl b
handleError either =
  case either of
    Left err -> (liftIO . print) err >> abort
    Right a  -> return a

-- -------------------------------------------------------------------------------
-- -- Commands
-- -------------------------------------------------------------------------------

-- :browse command
browse :: String -> Repl ()
browse _ = do
  IState { typeEnv = tpEnv } <- get

  liftIO $ render (prettyEnv tpEnv)

-- :load command
load :: String -> Repl ()
load args = do
  contents <- liftIO $ L.readFile args
  exec contents

-- :type command
typeof :: String -> Repl ()
typeof arg = do
  st <- get
  let ctx = typeEnv st
      style = color Green <> bold
      strip = L.unpack . L.strip . L.pack
      arg' = strip arg
  case TypeEnv.lookup ctx arg' of
    Just val -> liftIO $ render $ prettyNamedScheme arg' val
    Nothing  -> return ()

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

-- -------------------------------------------------------------------------------
-- -- Interactive Shell
-- -------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  ctx <- get
  let cmds = commandNames
      defs = definitions ctx

  return $ filter (isPrefixOf n) (cmds ++ defs)

commandNames :: [String]
commandNames = map ((\s -> ":" <> s) . fst) shellOptions

shellOptions :: [(String, String -> Repl ())]
shellOptions = [
    ("load"   , load)        -- :load
  , ("browse" , browse)      -- :browse
  , ("quit"   , quit)        -- :quit
  , ("type"   , typeof)      -- :type
  ]

say :: String -> Repl ()
say words = do
  liftIO $ render . pretty $ words

asciiArt =
  "    ______            __                          \n\
  \   / ____/___  ____  / /   ____ _____  _____      \n\
  \  / /_  / __ \\/ __ \\/ /   / __ `/ __ \\/ __ `/     \n\
  \ / __/ / /_/ / /_/ / /___/ /_/ / / / / /_/ /      \n\
  \/_/    \\____/\\____/_____/\\__,_/_/ /_/\\__, /       \n\
  \                                    /____/        \n"



shellWelcome :: Repl ()
shellWelcome = say asciiArt >> say "-- Welcome to FooLang"

shellGoodbye :: Repl ExitDecision
shellGoodbye = say "Bye bye" >> return Exit

shellBanner :: MultiLine -> Repl String
shellBanner SingleLine = pure "FooLang> "
shellBanner MultiLine  = pure "| "

shellAction :: String -> Repl ()
shellAction source = exec (L.pack source)

-- -------------------------------------------------------------------------------
-- -- Entry Point
-- -------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell action =
  (flip evalStateT) initState $
  evalReplOpts $ ReplOpts {
    banner = shellBanner,
    command = shellAction,
    options = shellOptions,
    prefix  = Just ':',
    multilineCommand = Just "paste",
    tabComplete = completer,
    initialiser = shellWelcome >> action >> return (),
    finaliser = shellGoodbye
  }

-----------------------------------------------------------------------------
-- Toplevel
-- -------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []              -> shell (return ())
    [fname]         -> shell (load fname)
    ["test", fname] -> shell (load fname >> browse [] >> quit ())
    _               -> putStrLn "invalid arguments"

