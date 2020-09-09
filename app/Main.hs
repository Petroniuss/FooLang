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
import qualified Data.Text.Lazy                                                   as T
import qualified Data.Text.Lazy.IO                                                as T

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
                                                                                   Pretty (pretty),
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

data ShellState = ShellState
  { typeEnv :: TypeEnv.TypeEnv  -- Type environment
  , termEnv :: TermEnv          -- Value environment
  } deriving (Show)

initState :: ShellState
initState = ShellState
  { typeEnv = TypeEnv.emptyTypeEnv
  , termEnv = emptyTermEnv }

definitions :: ShellState -> [String]
definitions ShellState { termEnv = tEnv } = Map.keys tEnv

type Repl a = HaskelineT (StateT ShellState IO) a

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

-- Everything happens here!
exec :: T.Text -> Repl ()
exec source = do
  -- Get the current interpreter state
  ShellState { typeEnv = tpEnv, termEnv = tmEnv } <- get

  -- Parser ( returns AST )
  (ast, it) <- handleError $ parseModule "<stdin>" source

  -- Type Inference ( returns Typing Environment )
  tpEnv' <- handleError $ inferModule tpEnv ast

  -- Create updated environment
  let st' = ShellState { termEnv = foldl' evalDef tmEnv ast
                   , typeEnv = tpEnv' }
  -- Update environment
  put st'

  -- If a value is entered, evaluate and print it.
  case it of
    Nothing -> return ()
    Just ex -> do
      -- Eval expr and report error it it occurs
      tp <- handleTypeError $ inferIt tpEnv' ex
      -- This one cannot fail :)
      let val = evalExpr tmEnv ex

      -- Show evaluated value alongside its type
      replSuccess $ prettyIt val tp

-- Show success
replSuccess :: Doc AnsiStyle -> Repl ()
replSuccess doc = liftIO $ renderSuccess doc

-- Show failure
replFailure :: Doc AnsiStyle -> Repl ()
replFailure doc = liftIO $ renderFailure doc


handleError :: Show a => Either a b -> Repl b
handleError either =
  case either of
    Left err -> (replFailure . pretty . show) err >> abort
    Right a  -> return a

handleTypeError :: Either TypeError a -> Repl a
handleTypeError either =
  case either of
    Left typeErr -> (replFailure . pretty) typeErr >> abort
    Right a      -> return a

-- -------------------------------------------------------------------------------
-- -- Commands
-- -------------------------------------------------------------------------------

-- :browse command
browse :: String -> Repl ()
browse _ = do
  ShellState { typeEnv = tpEnv } <- get
  liftIO $ renderSuccess (prettyEnv tpEnv)

-- :load command
load :: String -> Repl ()
load args = do
  contents <- liftIO $ T.readFile args
  exec contents

-- :type command
typeof :: String -> Repl ()
typeof arg = do
  ShellState { typeEnv = env } <- get
  let arg' = strip arg
  case TypeEnv.lookup env arg' of
    Nothing  -> replFailure $ prettyDefNotFound arg'
    Just val -> replSuccess $ prettyNamedScheme arg' val

    where strip = T.unpack . T.strip . T.pack

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
comp :: (Monad m, MonadState ShellState m) => WordCompleter m
comp n = do
  ctx <- get
  let cmds = commandNames
      defs = definitions ctx

  return $ filter (isPrefixOf n) (cmds ++ defs)

commandNames :: [String]
commandNames = map ((\s -> ":" <> s) . fst) shellOptions

shellOptions :: [(String, String -> Repl ())]
shellOptions = [
    ("load"   , load)        -- :load   -> interpret file with source code
  , ("browse" , browse)      -- :browse -> browse all definitons
  , ("quit"   , quit)        -- :quit   -> exit shell
  , ("type"   , typeof)      -- :type   -> check type of definition
  ]

say :: String -> Repl ()
say words = do
  liftIO $ renderSuccess . pretty $ words

asciiArt :: String
asciiArt =
  "    ______            __                          \n\
  \   / ____/___  ____  / /   ____ _____  _____      \n\
  \  / /_  / __ \\/ __ \\/ /   / __ `/ __ \\/ __ `/     \n\
  \ / __/ / /_/ / /_/ / /___/ /_/ / / / / /_/ /      \n\
  \/_/    \\____/\\____/_____/\\__,_/_/ /_/\\__, /       \n\
  \                                    /____/        \n"

shellCompleter :: CompleterStyle (StateT ShellState IO)
shellCompleter = Prefix (wordCompleter comp) defaultMatcher

shellWelcome :: Repl ()
shellWelcome = say asciiArt >> say "-- Welcome to FooLang"

shellGoodbye :: Repl ExitDecision
shellGoodbye = say "Bye bye" >> return Exit

shellBanner :: MultiLine -> Repl String
shellBanner SingleLine = pure "FooLang> "
shellBanner MultiLine  = pure "| "

shellAction :: String -> Repl ()
shellAction source = exec (T.pack source)

-- -------------------------------------------------------------------------------
-- -- Entry Point
-- -------------------------------------------------------------------------------


shell :: Repl a -> IO ()
shell action =
  (flip evalStateT) initState $
  evalReplOpts $ ReplOpts {
    banner = shellBanner,
    command = shellAction,
    options = shellOptions,
    prefix  = Just ':',
    multilineCommand = Just "paste",
    tabComplete = shellCompleter,
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
    []      -> shell (return ())
    [fname] -> shell (load fname >> browse fname)
    _       -> renderFailure $ pretty ("Bad args" :: String)

