{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Argus.Runtime.Interpreter
-- Description : Runtime Haskell evaluation via hint
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides runtime Haskell evaluation for user-defined predicates
-- and transformations in TOML rules. It uses the hint package (GHC API wrapper)
-- to safely interpret Haskell code at runtime.
--
-- Example TOML usage:
--
-- @
-- [[rules.custom]]
-- id = "my-custom-rule"
-- pattern = "foo $X"
-- predicate = "\\ctx -> length (ecMatchedText ctx) > 10"
-- transform = "\\matched -> T.toUpper matched"
-- @
module Argus.Runtime.Interpreter
  ( -- * Runtime Management
    initRuntime
  , shutdownRuntime
  , withRuntime

    -- * Predicate Evaluation
  , evalPredicate
  , evalPredicateIO
  , compilePredicate
  , CompiledPredicate (..)

    -- * Transform Evaluation
  , evalTransform
  , evalTransformIO
  , compileTransform
  , CompiledTransform (..)

    -- * Module Loading
  , loadUserModule
  , loadUserModuleIO
  , reloadModules
  , unloadModule
  , listLoadedModules

    -- * Expression Evaluation
  , evalExpr
  , evalExprTyped
  , typeOfExpr

    -- * Utilities
  , validateCode
  , getTypeSignature
  , browseModule

    -- * Internal types (exported to silence unused warnings)
  , CachedExpr(..)
  ) where

import Control.Concurrent.MVar
import Control.Monad (forM, when)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Language.Haskell.Interpreter qualified as Hint
import System.Directory (doesFileExist, getModificationTime)
import System.Timeout (timeout)

import Argus.Runtime.Types

--------------------------------------------------------------------------------
-- Runtime State
--------------------------------------------------------------------------------

-- | Global runtime state (thread-safe via MVar)
data RuntimeState = RuntimeState
  { rsEnv :: RuntimeEnv
  , rsSessionLock :: MVar ()  -- Lock for interpreter session
  , rsCache :: IORef (Map Text CachedExpr)
  }

-- | Cached compiled expression
data CachedExpr = CachedExpr
  { _ceType :: Text
  , _ceCompileTime :: Double
  }

-- | Global runtime reference
type RuntimeRef = MVar RuntimeState

--------------------------------------------------------------------------------
-- Compiled Types
--------------------------------------------------------------------------------

-- | A compiled predicate ready for execution
newtype CompiledPredicate = CompiledPredicate
  { runPredicate :: EvalContext -> IO (RuntimeResult Bool)
  }

-- | A compiled transform ready for execution
newtype CompiledTransform = CompiledTransform
  { runTransform :: Text -> IO (RuntimeResult Text)
  }

--------------------------------------------------------------------------------
-- Runtime Initialization
--------------------------------------------------------------------------------

-- | Initialize the runtime environment
initRuntime :: RuntimeConfig -> IO (Either RuntimeError RuntimeRef)
initRuntime cfg = do
  now <- getCurrentTime
  let env = RuntimeEnv
        { reConfig = cfg
        , reLoadedModules = Map.empty
        , reImportedModules = Set.fromList (rcDefaultImports cfg)
        , reCapabilities = if rcSandboxed cfg
                            then restrictedCapabilities
                            else defaultCapabilities
        , reInitTime = now
        }

  sessionLock <- newMVar ()
  cacheRef <- newIORef Map.empty

  let state = RuntimeState
        { rsEnv = env
        , rsSessionLock = sessionLock
        , rsCache = cacheRef
        }

  ref <- newMVar state

  -- Verify the interpreter can be initialized
  result <- verifyInterpreter ref
  case result of
    Left err -> return $ Left err
    Right () -> return $ Right ref

-- | Verify interpreter can be initialized
verifyInterpreter :: RuntimeRef -> IO (Either RuntimeError ())
verifyInterpreter ref = withMVar ref $ \state -> do
  let cfg = reConfig (rsEnv state)
  withMVar (rsSessionLock state) $ \() -> do
    result <- runInterpreter cfg $ do
      Hint.set [Hint.languageExtensions Hint.:= defaultExtensions]
      Hint.setImportsQ $ map (\m -> (T.unpack m, Nothing)) (rcDefaultImports cfg)
    case result of
      Left err -> return $ Left $ hintErrorToRuntime err
      Right () -> return $ Right ()
  where
    defaultExtensions =
      [ Hint.OverloadedStrings
      , Hint.LambdaCase
      , Hint.TupleSections
      , Hint.TypeApplications
      ]

-- | Shutdown the runtime and release resources
shutdownRuntime :: RuntimeRef -> IO ()
shutdownRuntime ref = modifyMVar_ ref $ \state -> do
  -- Just clear the cache, no session to close
  writeIORef (rsCache state) Map.empty
  return state

-- | Run an action with a runtime, ensuring cleanup
withRuntime :: RuntimeConfig -> (RuntimeRef -> IO a) -> IO (Either RuntimeError a)
withRuntime cfg action = do
  initResult <- initRuntime cfg
  case initResult of
    Left err -> return $ Left err
    Right ref -> do
      result <- action ref
      shutdownRuntime ref
      return $ Right result

--------------------------------------------------------------------------------
-- Predicate Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a predicate against a context (pure interface)
evalPredicate :: RuntimeRef -> PredicateCode -> EvalContext -> IO (RuntimeResult Bool)
evalPredicate ref (PredicateCode code) ctx = do
  evalPredicateIO ref code ctx

-- | Evaluate a predicate (IO interface)
evalPredicateIO :: RuntimeRef -> Text -> EvalContext -> IO (RuntimeResult Bool)
evalPredicateIO ref code ctx = withMVar ref $ \state -> do
  let cfg = reConfig (rsEnv state)
      timeoutMs = rcTimeout cfg * 1000  -- convert to microseconds

  result <- timeout timeoutMs $ runInterpreter cfg $ do
    setupImports cfg
    let fullExpr = "(" <> T.unpack code <> ") ctx"
        ctxExpr = "let ctx = " <> show ctx <> " in " <> fullExpr
    Hint.interpret ctxExpr (Hint.as :: Bool)

  case result of
    Nothing -> return $ RuntimeFailure $ RuntimeError
      { reKind = TimeoutError
      , reMessage = "Predicate evaluation timed out"
      , reLocation = Nothing
      , reContext = Just code
      , reSuggestion = Just "Simplify the predicate or increase timeout"
      }
    Just (Left err) -> return $ RuntimeFailure $ hintErrorToRuntime err
    Just (Right val) -> return $ RuntimeSuccess val

-- | Compile a predicate for repeated execution
compilePredicate :: RuntimeRef -> PredicateCode -> IO (RuntimeResult CompiledPredicate)
compilePredicate ref (PredicateCode code) = do
  -- Validate the code first
  validationResult <- validateCode ref code "EvalContext -> Bool"
  case validationResult of
    RuntimeFailure err -> return $ RuntimeFailure err
    RuntimeSuccess _ -> return $ RuntimeSuccess $ CompiledPredicate $ \ctx ->
      evalPredicateIO ref code ctx

--------------------------------------------------------------------------------
-- Transform Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a transform against matched text
evalTransform :: RuntimeRef -> TransformCode -> Text -> IO (RuntimeResult Text)
evalTransform ref (TransformCode code) input =
  evalTransformIO ref code input

-- | Evaluate a transform (IO interface)
evalTransformIO :: RuntimeRef -> Text -> Text -> IO (RuntimeResult Text)
evalTransformIO ref code input = withMVar ref $ \state -> do
  let cfg = reConfig (rsEnv state)
      timeoutMs = rcTimeout cfg * 1000

  result <- timeout timeoutMs $ runInterpreter cfg $ do
    setupImports cfg
    let fullExpr = "(" <> T.unpack code <> ") input"
        inputExpr = "let input = " <> show input <> " :: Data.Text.Text in " <> fullExpr
    Hint.interpret inputExpr (Hint.as :: String)

  case result of
    Nothing -> return $ RuntimeFailure $ RuntimeError
      { reKind = TimeoutError
      , reMessage = "Transform evaluation timed out"
      , reLocation = Nothing
      , reContext = Just code
      , reSuggestion = Just "Simplify the transform or increase timeout"
      }
    Just (Left err) -> return $ RuntimeFailure $ hintErrorToRuntime err
    Just (Right val) -> return $ RuntimeSuccess (T.pack val)

-- | Compile a transform for repeated execution
compileTransform :: RuntimeRef -> TransformCode -> IO (RuntimeResult CompiledTransform)
compileTransform ref (TransformCode code) = do
  validationResult <- validateCode ref code "Text -> Text"
  case validationResult of
    RuntimeFailure err -> return $ RuntimeFailure err
    RuntimeSuccess _ -> return $ RuntimeSuccess $ CompiledTransform $ \input ->
      evalTransformIO ref code input

--------------------------------------------------------------------------------
-- Module Loading
--------------------------------------------------------------------------------

-- | Load a user module from a file or inline source
loadUserModule :: RuntimeRef -> ModuleSource -> IO (RuntimeResult LoadedModule)
loadUserModule ref source = loadUserModuleIO ref source

-- | Load a user module (IO interface)
loadUserModuleIO :: RuntimeRef -> ModuleSource -> IO (RuntimeResult LoadedModule)
loadUserModuleIO ref source = modifyMVar ref $ \state -> do
  now <- getCurrentTime
  case source of
    ModuleFile path -> do
      exists <- doesFileExist path
      if not exists
        then return (state, RuntimeFailure $ RuntimeError
          { reKind = ModuleNotFound
          , reMessage = "Module file not found: " <> T.pack path
          , reLocation = Just (T.pack path)
          , reContext = Nothing
          , reSuggestion = Just "Check that the file path is correct"
          })
        else do
          modTime <- getModificationTime path
          let cfg = reConfig (rsEnv state)

          result <- runInterpreter cfg $ do
            Hint.loadModules [path]
            exports <- Hint.getModuleExports path
            return exports

          case result of
            Left err -> return (state, RuntimeFailure $ hintErrorToRuntime err)
            Right exports -> do
              let modName = T.pack $ takeModuleName path
                  loaded = LoadedModule
                    { lmPath = path
                    , lmName = modName
                    , lmExports = map (T.pack . show) exports
                    , lmStatus = ModuleLoaded
                    , lmLoadTime = now
                    , lmChecksum = T.pack $ show modTime
                    }
                  env' = (rsEnv state)
                    { reLoadedModules = Map.insert path loaded (reLoadedModules (rsEnv state))
                    , reImportedModules = Set.insert modName (reImportedModules (rsEnv state))
                    }
              return (state { rsEnv = env' }, RuntimeSuccess loaded)

    ModuleInline modName content -> do
      let cfg = reConfig (rsEnv state)

      result <- runInterpreter cfg $ do
        -- For inline modules, we use runStmt to define them
        Hint.runStmt $ T.unpack content
        return ()

      case result of
        Left err -> return (state, RuntimeFailure $ hintErrorToRuntime err)
        Right () -> do
          let loaded = LoadedModule
                { lmPath = "<inline>"
                , lmName = modName
                , lmExports = []
                , lmStatus = ModuleLoaded
                , lmLoadTime = now
                , lmChecksum = T.pack $ show $ T.length content
                }
              env' = (rsEnv state)
                { reLoadedModules = Map.insert (T.unpack modName) loaded (reLoadedModules (rsEnv state))
                }
          return (state { rsEnv = env' }, RuntimeSuccess loaded)

-- | Extract module name from file path
takeModuleName :: FilePath -> String
takeModuleName path =
  let base = reverse $ takeWhile (/= '/') $ reverse path
  in takeWhile (/= '.') base

-- | Reload all loaded modules (check for changes)
reloadModules :: RuntimeRef -> IO [RuntimeResult LoadedModule]
reloadModules ref = withMVar ref $ \state -> do
  let modules = Map.toList $ reLoadedModules (rsEnv state)
  forM modules $ \(path, lm) -> do
    if lmPath lm == "<inline>"
      then return $ RuntimeSuccess lm
      else do
        exists <- doesFileExist path
        if not exists
          then return $ RuntimeFailure $ RuntimeError
            { reKind = ModuleNotFound
            , reMessage = "Module file no longer exists: " <> T.pack path
            , reLocation = Just (T.pack path)
            , reContext = Nothing
            , reSuggestion = Just "The file may have been moved or deleted"
            }
          else do
            modTime <- getModificationTime path
            let currentChecksum = T.pack $ show modTime
            if currentChecksum == lmChecksum lm
              then return $ RuntimeSuccess lm
              else loadUserModule ref (ModuleFile path)

-- | Unload a module
unloadModule :: RuntimeRef -> FilePath -> IO ()
unloadModule ref path = modifyMVar_ ref $ \state -> do
  let env' = (rsEnv state)
        { reLoadedModules = Map.delete path (reLoadedModules (rsEnv state))
        }
  return state { rsEnv = env' }

-- | List all loaded modules
listLoadedModules :: RuntimeRef -> IO [LoadedModule]
listLoadedModules ref = withMVar ref $ \state ->
  return $ Map.elems $ reLoadedModules (rsEnv state)

--------------------------------------------------------------------------------
-- Expression Evaluation
--------------------------------------------------------------------------------

-- | Evaluate an arbitrary Haskell expression
evalExpr :: RuntimeRef -> Text -> IO (RuntimeResult Text)
evalExpr ref expr = withMVar ref $ \state -> do
  let cfg = reConfig (rsEnv state)
      timeoutMs = rcTimeout cfg * 1000

  result <- timeout timeoutMs $ runInterpreter cfg $ do
    setupImports cfg
    Hint.eval (T.unpack expr)

  case result of
    Nothing -> return $ RuntimeFailure $ RuntimeError
      { reKind = TimeoutError
      , reMessage = "Expression evaluation timed out"
      , reLocation = Nothing
      , reContext = Just expr
      , reSuggestion = Just "Simplify the expression or increase timeout"
      }
    Just (Left err) -> return $ RuntimeFailure $ hintErrorToRuntime err
    Just (Right val) -> return $ RuntimeSuccess (T.pack val)

-- | Evaluate an expression with expected type
evalExprTyped :: forall a. (Typeable a) => RuntimeRef -> Text -> IO (RuntimeResult a)
evalExprTyped ref expr = withMVar ref $ \state -> do
  let cfg = reConfig (rsEnv state)
      timeoutMs = rcTimeout cfg * 1000

  result <- timeout timeoutMs $ runInterpreter cfg $ do
    setupImports cfg
    Hint.interpret (T.unpack expr) (Hint.as :: a)

  case result of
    Nothing -> return $ RuntimeFailure $ RuntimeError
      { reKind = TimeoutError
      , reMessage = "Expression evaluation timed out"
      , reLocation = Nothing
      , reContext = Just expr
      , reSuggestion = Nothing
      }
    Just (Left err) -> return $ RuntimeFailure $ hintErrorToRuntime err
    Just (Right val) -> return $ RuntimeSuccess val

-- | Get the type of an expression
typeOfExpr :: RuntimeRef -> Text -> IO (RuntimeResult Text)
typeOfExpr ref expr = withMVar ref $ \state -> do
  let cfg = reConfig (rsEnv state)

  result <- runInterpreter cfg $ do
    setupImports cfg
    Hint.typeOf (T.unpack expr)

  case result of
    Left err -> return $ RuntimeFailure $ hintErrorToRuntime err
    Right typ -> return $ RuntimeSuccess (T.pack typ)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Validate code before execution
validateCode :: RuntimeRef -> Text -> Text -> IO (RuntimeResult ())
validateCode ref code expectedType = withMVar ref $ \state -> do
  let cfg = reConfig (rsEnv state)
      caps = reCapabilities (rsEnv state)

  -- Check for forbidden functions
  let forbidden = Set.toList $ capForbiddenFunctions caps
      codeStr = T.unpack code
      violations = filter (`elem` words codeStr) (map T.unpack forbidden)

  if not (null violations)
    then return $ RuntimeFailure $ RuntimeError
      { reKind = SecurityViolation
      , reMessage = "Code contains forbidden function(s): " <> T.pack (show violations)
      , reLocation = Nothing
      , reContext = Just code
      , reSuggestion = Just "Remove unsafe operations from the code"
      }
    else do
      -- Type check the expression
      result <- runInterpreter cfg $ do
        setupImports cfg
        actualType <- Hint.typeOf (T.unpack code)
        return actualType

      case result of
        Left err -> return $ RuntimeFailure $ hintErrorToRuntime err
        Right actualType ->
          if matchesType (T.pack actualType) expectedType
            then return $ RuntimeSuccess ()
            else return $ RuntimeFailure $ RuntimeError
              { reKind = TypeCheckError
              , reMessage = "Type mismatch: expected " <> expectedType <> ", got " <> T.pack actualType
              , reLocation = Nothing
              , reContext = Just code
              , reSuggestion = Just $ "Ensure the expression has type " <> expectedType
              }

-- | Simple type matching (handles some common variations)
matchesType :: Text -> Text -> Bool
matchesType actual expected =
  T.strip actual == T.strip expected ||
  normalizeType actual == normalizeType expected
  where
    normalizeType t = T.replace "Data.Text.Internal.Text" "Text" $
                     T.replace "GHC.Base.String" "String" $
                     T.replace "[Char]" "String" t

-- | Get the type signature of an expression
getTypeSignature :: RuntimeRef -> Text -> IO (RuntimeResult Text)
getTypeSignature = typeOfExpr

-- | Browse a module's exports
browseModule :: RuntimeRef -> Text -> IO (RuntimeResult [Text])
browseModule ref modName = withMVar ref $ \state -> do
  let cfg = reConfig (rsEnv state)

  result <- runInterpreter cfg $ do
    Hint.setImportsQ [(T.unpack modName, Nothing)]
    exports <- Hint.getModuleExports (T.unpack modName)
    return $ map (T.pack . show) exports

  case result of
    Left err -> return $ RuntimeFailure $ hintErrorToRuntime err
    Right exports -> return $ RuntimeSuccess exports

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Run the interpreter with config
runInterpreter :: RuntimeConfig -> Hint.Interpreter a -> IO (Either Hint.InterpreterError a)
runInterpreter cfg action = do
  let searchPaths = rcSearchPaths cfg
  Hint.runInterpreter $ do
    when (not $ null searchPaths) $
      Hint.set [Hint.searchPath Hint.:= searchPaths]
    action

-- | Setup default imports
setupImports :: RuntimeConfig -> Hint.Interpreter ()
setupImports cfg = do
  Hint.setImportsQ $ defaultImportsQ ++ customImportsQ
  where
    defaultImportsQ =
      [ ("Prelude", Nothing)
      , ("Data.Text", Just "T")
      , ("Data.Map.Strict", Just "Map")
      , ("Data.Set", Just "Set")
      , ("Data.List", Nothing)
      , ("Data.Maybe", Nothing)
      , ("Data.Char", Nothing)
      , ("Control.Monad", Nothing)
      ]
    customImportsQ = map (\m -> (T.unpack m, Nothing)) $
                     filter (`notElem` map T.pack ["Prelude", "Data.Text", "Data.Map.Strict", "Data.Set", "Data.List", "Data.Maybe", "Data.Char", "Control.Monad"]) $
                     rcDefaultImports cfg

-- | Convert hint error to runtime error
hintErrorToRuntime :: Hint.InterpreterError -> RuntimeError
hintErrorToRuntime err = RuntimeError
  { reKind = errorKind
  , reMessage = T.pack $ show err
  , reLocation = Nothing
  , reContext = Nothing
  , reSuggestion = suggestion
  }
  where
    (errorKind, suggestion) = case err of
      Hint.UnknownError msg ->
        (InternalError, Just $ T.pack $ "Unknown error: " <> msg)
      Hint.WontCompile errs ->
        (TypeCheckError, Just $ T.pack $ "Compilation errors: " <> show (map Hint.errMsg errs))
      Hint.NotAllowed msg ->
        (SecurityViolation, Just $ T.pack $ "Not allowed: " <> msg)
      Hint.GhcException msg ->
        (InternalError, Just $ T.pack $ "GHC exception: " <> msg)
