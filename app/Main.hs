{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Lang.Eval
import           Lang.Parser
import           Lang.Pretty
import           Lang.Syntax
import qualified Lang.TypeEnv                          as TypeEnv
import           Lang.TypeInference.Infer
import           Lang.TypeInference.Type

import qualified Data.Map                              as Map
import           Data.Monoid
import qualified Data.Text.Lazy                        as T
import qualified Data.Text.Lazy.IO                     as T

import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           Control.Arrow                         (left)
import           Data.List                             (foldl', isPrefixOf)

import           Data.Text.Prettyprint.Doc             (Doc, Pretty (pretty),
                                                        (<+>))

import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Prettyprinter.Render.Terminal         (AnsiStyle)
import           System.Console.Haskeline
import           System.Console.Repline
import           System.Environment
import           System.Exit
import           Text.Parsec.Error                     (ParseError)


-------------------------------------------------------------------------------
-- Shell State
-------------------------------------------------------------------------------

data ShellState = ShellState
  { typeEnv :: TypeEnv.TypeEnv  -- Type environment
  , termEnv :: TermEnv          -- Value environment
  }

initState :: ShellState
initState = ShellState
  { typeEnv = TypeEnv.emptyTypeEnv
  , termEnv = emptyTermEnv }

definitions :: ShellState -> [String]
definitions ShellState { termEnv = tEnv } = Map.keys tEnv

type Shell a = HaskelineT (StateT ShellState IO) a

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

-- Everything happens here!
exec :: T.Text -> Shell ()
exec source = do
  -- Get the current interpreter state
  ShellState { typeEnv = tpEnv, termEnv = tmEnv } <- get

  -- Parser ( returns AST )
  (ast, it) <- handleParseError $ parseModule "<stdin>" source

  -- Type Inference ( returns Typing Environment )
  tpEnv' <- handleTypeError $ inferModule tpEnv ast

  -- Create updated environment
  let st' = ShellState { termEnv = foldl' evalDef tmEnv ast
                       , typeEnv = tpEnv' }
  -- Update environment
  put st'

  -- If an expression is entered then typecheck, evaluate and print.
  case it of
    Nothing -> return ()
    Just ex -> do
      -- Eval expr and report error if it occurs
      tp <- handleTypeError $ inferIt tpEnv' ex
      -- This one cannot fail :)
      let val = evalExpr tmEnv ex

      -- Show evaluated value alongside its type
      shellSuccess $ prettyIt val tp

-- -------------------------------------------------------------------------------
-- -- Errors
-- -------------------------------------------------------------------------------

handleParseError :: Either ParseError b -> Shell b
handleParseError = handleErrorPretty . (left show)

handleTypeError :: Either TypeError a -> Shell a
handleTypeError = handleErrorPretty

handleErrorPretty :: Pretty a => Either a b -> Shell b
handleErrorPretty either =
  case either of
    Left err -> (shellFailue . pretty) err >> abort
    Right a  -> return a

-- -------------------------------------------------------------------------------
-- -- Commands
-- -------------------------------------------------------------------------------

-- :browse command
browse :: String -> Shell ()
browse _ = do
  ShellState { typeEnv = tpEnv } <- get
  liftIO . renderSuccess . pretty $ tpEnv

-- :load command
load :: String -> Shell ()
load args = do
  contents <- liftIO $ T.readFile args
  exec contents

-- :type command
typeof :: String -> Shell ()
typeof arg = do
  ShellState { typeEnv = env } <- get
  let arg' = strip arg
  case TypeEnv.lookup env arg' of
    Nothing  -> shellFailue $ prettyDefNotFound arg'
    Just val -> shellSuccess $ prettyNamedScheme arg' val

    where strip = T.unpack . T.strip . T.pack

-- :quit command
quit :: a -> Shell ()
quit _ = liftIO $ exitSuccess

-- -------------------------------------------------------------------------------
-- Shell
-- -------------------------------------------------------------------------------

-- | High level interface for creating a shell - for more documentation refer to hackage.
shell :: Shell a -> IO ()
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

-- |Show success
shellSuccess :: Doc AnsiStyle -> Shell ()
shellSuccess doc = liftIO . renderSuccess $ doc

-- |Show failure
shellFailue :: Doc AnsiStyle -> Shell ()
shellFailue doc = liftIO . renderFailure $ doc

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  ]

comp :: (Monad m, MonadState ShellState m) => WordCompleter m
comp n = do
  ctx <- get
  let cmds = commandNames
      defs = definitions ctx

  return $ filter (isPrefixOf n) (cmds ++ defs)

commandNames :: [String]
commandNames = pasteOption : map ((\s -> ":" <> s) . fst) shellOptions
  where pasteOption = ":paste"

shellOptions :: [(String, String -> Shell ())]
shellOptions = [
    ("load"   , load)        -- :load   -> interpret file with source code
  , ("browse" , browse)      -- :browse -> browse all definitons
  , ("quit"   , quit)        -- :quit   -> exit shell
  , ("type"   , typeof)      -- :type   -> check type of definition
  ]

say :: String -> Shell ()
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

shellWelcome :: Shell ()
shellWelcome = say asciiArt >> say "-- Welcome to FooLang"

shellGoodbye :: Shell ExitDecision
shellGoodbye = say "Bye bye" >> return Exit

shellBanner :: MultiLine -> Shell String
shellBanner SingleLine = pure "FooLang> "
shellBanner MultiLine  = pure "| "

shellAction :: String -> Shell ()
shellAction source = exec (T.pack source)

-----------------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> shell (return ())
    [fname] -> shell (load fname >> browse fname)
    _       -> renderFailure $ pretty ("Bad args" :: String)

