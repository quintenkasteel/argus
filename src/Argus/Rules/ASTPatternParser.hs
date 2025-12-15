{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Rules.ASTPatternParser
-- Description : Parser for AST patterns in TOML rules
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides parsing for a simplified AST pattern language that can
-- be used in TOML rule definitions. The pattern language supports matching
-- Haskell AST structures without requiring users to understand GHC internals.
--
-- Example patterns:
--
-- @
-- -- Match lambda with unused parameter
-- \\$x -> $body where $x `notFreeIn` $body
--
-- -- Match if-then-else returning Bool
-- if $cond then $t else $f where $t :: Bool && $f :: Bool
--
-- -- Match function application
-- $f $x $y where $f :: $a -> $b -> $c
-- @
module Argus.Rules.ASTPatternParser
  ( -- * AST Pattern Types
    ASTPattern (..)
  , PatternConstraint (..)
  , ConstraintOp (..)
  , LiteralPattern (..)
  , TypePattern (..)
  , DoStatement (..)

    -- * Pattern Parsing
  , parseASTPattern
  , parsePatternConstraints
  , ParseError (..)

    -- * Pattern Matching
  , matchASTPattern
  , MatchResult (..)
  , MatchBinding (..)

    -- * Pattern Utilities
  , patternVars
  , patternComplexity
  , simplifyPattern
  , normalizePattern

    -- * Pretty Printing
  , ppPattern
  , ppConstraint
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isUpper)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec hiding (ParseError, match)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------
-- AST Pattern Types
--------------------------------------------------------------------------------

-- | An AST pattern for matching Haskell code
data ASTPattern
  = PVar Text
      -- ^ A metavariable ($x, $f, $body)
  | PWild
      -- ^ Wildcard pattern (_)
  | PLit LiteralPattern
      -- ^ Literal pattern (number, string, char)
  | PApp ASTPattern ASTPattern
      -- ^ Function application (f x)
  | PLam [Text] ASTPattern
      -- ^ Lambda (\\x y -> body)
  | PLet [(Text, ASTPattern)] ASTPattern
      -- ^ Let binding (let x = e in body)
  | PIf ASTPattern ASTPattern ASTPattern
      -- ^ If-then-else
  | PCase ASTPattern [(ASTPattern, ASTPattern)]
      -- ^ Case expression
  | PTuple [ASTPattern]
      -- ^ Tuple pattern ((a, b, c))
  | PList [ASTPattern]
      -- ^ List pattern ([a, b, c])
  | PCons ASTPattern ASTPattern
      -- ^ Cons pattern (x : xs)
  | PInfix ASTPattern Text ASTPattern
      -- ^ Infix operation (a + b)
  | PSection (Maybe ASTPattern) Text (Maybe ASTPattern)
      -- ^ Section ((+ 1), (1 +))
  | PDo [DoStatement]
      -- ^ Do notation
  | PTypeSig ASTPattern TypePattern
      -- ^ Type signature (e :: T)
  | PAsPat Text ASTPattern
      -- ^ As-pattern (x@p)
  | PBang ASTPattern
      -- ^ Bang pattern (!p)
  | PView ASTPattern ASTPattern
      -- ^ View pattern (f -> p)
  | PGuarded [(ASTPattern, ASTPattern)]
      -- ^ Guarded expressions
  | PConstrained ASTPattern [PatternConstraint]
      -- ^ Pattern with constraints
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Literal patterns
data LiteralPattern
  = LitInt Integer
  | LitFloat Double
  | LitChar Char
  | LitString Text
  | LitBool Bool
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Type pattern for matching types
data TypePattern
  = TVar Text
      -- ^ Type variable
  | TCon Text
      -- ^ Type constructor
  | TApp TypePattern TypePattern
      -- ^ Type application
  | TFun TypePattern TypePattern
      -- ^ Function type
  | TTuple [TypePattern]
      -- ^ Tuple type
  | TList TypePattern
      -- ^ List type
  | TForall [Text] TypePattern
      -- ^ Forall type
  | TConstraint [Text] TypePattern
      -- ^ Constrained type (Eq a => ...)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Do statement pattern
data DoStatement
  = DoExpr ASTPattern
      -- ^ Expression statement
  | DoBind Text ASTPattern
      -- ^ Bind statement (x <- e)
  | DoLet [(Text, ASTPattern)]
      -- ^ Let in do
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Constraints on pattern matches
data PatternConstraint
  = NotFreeIn Text Text
      -- ^ Variable not free in expression
  | FreeIn Text Text
      -- ^ Variable is free in expression
  | HasType Text TypePattern
      -- ^ Expression has type
  | HasTypeClass Text Text
      -- ^ Expression has typeclass constraint
  | NotEqual Text Text
      -- ^ Two expressions are not equal
  | IsAtomic Text
      -- ^ Expression is atomic (literal or variable)
  | IsPure Text
      -- ^ Expression is pure (no IO)
  | IsMonadic Text
      -- ^ Expression is monadic
  | ComplexityLT Text Int
      -- ^ Complexity less than
  | ComplexityGT Text Int
      -- ^ Complexity greater than
  | MatchesRegex Text Text
      -- ^ Text matches regex
  | InModule Text
      -- ^ Only match in specific module
  | NotInModule Text
      -- ^ Don't match in specific module
  | HasPragma Text
      -- ^ File has pragma
  | HasImport Text
      -- ^ File imports module
  | ConstraintAnd PatternConstraint PatternConstraint
      -- ^ Conjunction
  | ConstraintOr PatternConstraint PatternConstraint
      -- ^ Disjunction
  | ConstraintNot PatternConstraint
      -- ^ Negation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Constraint operators for parsing
data ConstraintOp
  = OpNotFreeIn
  | OpFreeIn
  | OpHasType
  | OpHasClass
  | OpNotEqual
  | OpIsAtomic
  | OpIsPure
  | OpIsMonadic
  deriving stock (Eq, Show, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
-- Parser Types
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

-- | Parse error type
data ParseError = ParseError
  { peMessage :: Text
  , peLocation :: Maybe (Int, Int)
  , peInput :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

--------------------------------------------------------------------------------
-- Pattern Parsing
--------------------------------------------------------------------------------

-- | Parse an AST pattern from text
parseASTPattern :: Text -> Either ParseError ASTPattern
parseASTPattern input =
  case M.parse (spaceConsumer *> patternP <* eof) "" input of
    Left err -> Left $ ParseError
      { peMessage = T.pack $ M.errorBundlePretty err
      , peLocation = getErrorLocation err
      , peInput = input
      }
    Right pat -> Right pat
  where
    getErrorLocation :: M.ParseErrorBundle Text Void -> Maybe (Int, Int)
    getErrorLocation bundle =
      let pos = M.pstateSourcePos $ M.bundlePosState bundle
      in Just (M.unPos $ M.sourceLine pos, M.unPos $ M.sourceColumn pos)

-- | Parse pattern constraints
parsePatternConstraints :: Text -> Either ParseError [PatternConstraint]
parsePatternConstraints input =
  case M.parse (spaceConsumer *> constraintsP <* eof) "" input of
    Left err -> Left $ ParseError
      { peMessage = T.pack $ M.errorBundlePretty err
      , peLocation = Nothing
      , peInput = input
      }
    Right cs -> Right cs

--------------------------------------------------------------------------------
-- Parser Implementation
--------------------------------------------------------------------------------

-- | Space consumer (handles whitespace and comments)
spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- | Parse a pattern
patternP :: Parser ASTPattern
patternP = do
  pat <- patternExprP
  -- Check for 'where' clause
  mConstraints <- optional $ do
    void $ symbol "where"
    constraintsP
  case mConstraints of
    Nothing -> return pat
    Just cs -> return $ PConstrained pat cs

-- | Parse pattern expression (handles infix)
patternExprP :: Parser ASTPattern
patternExprP = do
  left <- patternAppP
  rest <- many $ try $ do
    op <- lexeme operatorP
    right <- patternAppP
    return (op, right)
  return $ foldl' (\l (op, r) -> PInfix l op r) left rest

-- | Parse an operator
operatorP :: Parser Text
operatorP = T.pack <$> some (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: String))

-- | Parse function application
patternAppP :: Parser ASTPattern
patternAppP = do
  atoms <- some atomP
  return $ foldl1 PApp atoms

-- | Parse atomic pattern
atomP :: Parser ASTPattern
atomP = choice
  [ try wildcardP
  , try metavarP
  , try literalP
  , try lambdaP
  , try letP
  , try ifP
  , try caseP
  , try doP
  , try sectionP
  , try tupleOrParenP
  , try listP
  , try asPatternP
  , try bangPatternP
  , constructorOrVarP
  ]

-- | Parse wildcard (_)
wildcardP :: Parser ASTPattern
wildcardP = PWild <$ symbol "_"

-- | Parse metavariable ($x)
metavarP :: Parser ASTPattern
metavarP = do
  void $ char '$'
  name <- T.pack <$> ((:) <$> lowerChar <*> many alphaNumChar)
  spaceConsumer
  return $ PVar name

-- | Parse literal
literalP :: Parser ASTPattern
literalP = PLit <$> choice
  [ try $ LitFloat <$> lexeme L.float
  , try $ LitInt <$> lexeme L.decimal
  , try $ LitChar <$> lexeme charLiteral
  , try $ LitString . T.pack <$> lexeme stringLiteral
  , try $ LitBool True <$ symbol "True"
  , try $ LitBool False <$ symbol "False"
  ]
  where
    charLiteral = between (char '\'') (char '\'') L.charLiteral
    stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

-- | Parse lambda
lambdaP :: Parser ASTPattern
lambdaP = do
  void $ symbol "\\" <|> symbol "λ"
  params <- some $ lexeme (T.pack <$> ((:) <$> lowerChar <*> many alphaNumChar))
  void $ symbol "->"
  body <- patternP
  return $ PLam params body

-- | Parse let expression
letP :: Parser ASTPattern
letP = do
  void $ symbol "let"
  bindings <- bindingsP
  void $ symbol "in"
  body <- patternP
  return $ PLet bindings body
  where
    bindingsP = sepBy1 bindingP (symbol ";")
    bindingP = do
      name <- lexeme (T.pack <$> ((:) <$> lowerChar <*> many alphaNumChar))
      void $ symbol "="
      expr <- patternExprP
      return (name, expr)

-- | Parse if-then-else
ifP :: Parser ASTPattern
ifP = do
  void $ symbol "if"
  cond <- patternExprP
  void $ symbol "then"
  thenBranch <- patternExprP
  void $ symbol "else"
  elseBranch <- patternExprP
  return $ PIf cond thenBranch elseBranch

-- | Parse case expression
caseP :: Parser ASTPattern
caseP = do
  void $ symbol "case"
  scrutinee <- patternExprP
  void $ symbol "of"
  alts <- some altP
  return $ PCase scrutinee alts
  where
    altP = do
      pat <- atomP
      void $ symbol "->"
      expr <- patternExprP
      void $ optional $ symbol ";"
      return (pat, expr)

-- | Parse do notation
doP :: Parser ASTPattern
doP = do
  void $ symbol "do"
  stmts <- some stmtP
  return $ PDo stmts
  where
    stmtP = choice
      [ try bindStmtP
      , try letStmtP
      , DoExpr <$> patternExprP
      ]

    bindStmtP = do
      name <- lexeme (T.pack <$> ((:) <$> lowerChar <*> many alphaNumChar))
      void $ symbol "<-"
      expr <- patternExprP
      return $ DoBind name expr

    letStmtP = do
      void $ symbol "let"
      bindings <- sepBy1 bindingP (symbol ";")
      return $ DoLet bindings

    bindingP = do
      name <- lexeme (T.pack <$> ((:) <$> lowerChar <*> many alphaNumChar))
      void $ symbol "="
      expr <- patternExprP
      return (name, expr)

-- | Parse section ((+ 1) or (1 +))
sectionP :: Parser ASTPattern
sectionP = between (symbol "(") (symbol ")") $ do
  choice
    [ try $ do  -- Left section (+ 1)
        op <- lexeme operatorP
        arg <- patternExprP
        return $ PSection Nothing op (Just arg)
    , try $ do  -- Right section (1 +)
        arg <- patternExprP
        op <- lexeme operatorP
        return $ PSection (Just arg) op Nothing
    ]

-- | Parse tuple or parenthesized expression
tupleOrParenP :: Parser ASTPattern
tupleOrParenP = between (symbol "(") (symbol ")") $ do
  first <- patternP
  rest <- many $ symbol "," *> patternP
  case rest of
    [] -> return first
    _ -> return $ PTuple (first : rest)

-- | Parse list
listP :: Parser ASTPattern
listP = between (symbol "[") (symbol "]") $ do
  elems <- sepBy patternP (symbol ",")
  return $ PList elems

-- | Parse as-pattern (x@p)
asPatternP :: Parser ASTPattern
asPatternP = do
  name <- lexeme (T.pack <$> ((:) <$> lowerChar <*> many alphaNumChar))
  void $ char '@'
  pat <- atomP
  return $ PAsPat name pat

-- | Parse bang pattern (!p)
bangPatternP :: Parser ASTPattern
bangPatternP = do
  void $ char '!'
  pat <- atomP
  return $ PBang pat

-- | Parse constructor or variable
constructorOrVarP :: Parser ASTPattern
constructorOrVarP = do
  first <- letterChar
  rest <- many alphaNumChar
  let name = T.pack (first : rest)
  spaceConsumer
  -- Check for type annotation
  mType <- optional $ try $ do
    void $ symbol "::"
    typePatternP
  case mType of
    Nothing -> return $ PVar name
    Just typ -> return $ PTypeSig (PVar name) typ

-- | Parse type pattern
typePatternP :: Parser TypePattern
typePatternP = do
  t <- typeAtomP
  rest <- optional $ try $ do
    void $ symbol "->"
    typePatternP
  case rest of
    Nothing -> return t
    Just r -> return $ TFun t r

-- | Parse atomic type
typeAtomP :: Parser TypePattern
typeAtomP = choice
  [ try $ between (symbol "(") (symbol ")") typeTupleP
  , try $ between (symbol "[") (symbol "]") (TList <$> typePatternP)
  , try typeConOrVarP
  ]
  where
    typeTupleP = do
      first <- typePatternP
      rest <- many $ symbol "," *> typePatternP
      case rest of
        [] -> return first
        _ -> return $ TTuple (first : rest)

    typeConOrVarP = do
      first <- letterChar
      rest <- many alphaNumChar
      let name = T.pack (first : rest)
      spaceConsumer
      if isUpper first
        then return $ TCon name
        else return $ TVar name

-- | Parse constraints
constraintsP :: Parser [PatternConstraint]
constraintsP = constraintP `sepBy1` (symbol "&&" <|> symbol ",")

-- | Parse a single constraint
constraintP :: Parser PatternConstraint
constraintP = choice
  [ try notFreeInP
  , try freeInP
  , try hasTypeP
  , try hasClassP
  , try notEqualP
  , try isAtomicP
  , try isPureP
  , try isMonadicP
  , try complexityLTP
  , try complexityGTP
  , try matchesRegexP
  , try inModuleP
  , try notInModuleP
  , try hasImportP
  , try hasPragmaP
  , try notConstraintP
  , between (symbol "(") (symbol ")") orConstraintP
  ]

-- | Parse `$x notFreeIn $body`
notFreeInP :: Parser PatternConstraint
notFreeInP = do
  var <- varNameP
  void $ symbol "`notFreeIn`" <|> symbol "notFreeIn"
  expr <- varNameP
  return $ NotFreeIn var expr

-- | Parse `$x freeIn $body`
freeInP :: Parser PatternConstraint
freeInP = do
  var <- varNameP
  void $ symbol "`freeIn`" <|> symbol "freeIn"
  expr <- varNameP
  return $ FreeIn var expr

-- | Parse `$x :: Type`
hasTypeP :: Parser PatternConstraint
hasTypeP = do
  var <- varNameP
  void $ symbol "::"
  typ <- typePatternP
  return $ HasType var typ

-- | Parse `$x hasClass Eq`
hasClassP :: Parser PatternConstraint
hasClassP = do
  var <- varNameP
  void $ symbol "hasClass"
  cls <- lexeme (T.pack <$> ((:) <$> upperChar <*> many alphaNumChar))
  return $ HasTypeClass var cls

-- | Parse `$x /= $y`
notEqualP :: Parser PatternConstraint
notEqualP = do
  var1 <- varNameP
  void $ symbol "/=" <|> symbol "!="
  var2 <- varNameP
  return $ NotEqual var1 var2

-- | Parse `isAtomic $x`
isAtomicP :: Parser PatternConstraint
isAtomicP = do
  void $ symbol "isAtomic"
  var <- varNameP
  return $ IsAtomic var

-- | Parse `isPure $x`
isPureP :: Parser PatternConstraint
isPureP = do
  void $ symbol "isPure"
  var <- varNameP
  return $ IsPure var

-- | Parse `isMonadic $x`
isMonadicP :: Parser PatternConstraint
isMonadicP = do
  void $ symbol "isMonadic"
  var <- varNameP
  return $ IsMonadic var

-- | Parse `complexity $x < 10`
complexityLTP :: Parser PatternConstraint
complexityLTP = do
  void $ symbol "complexity"
  var <- varNameP
  void $ symbol "<"
  n <- lexeme L.decimal
  return $ ComplexityLT var (fromInteger n)

-- | Parse `complexity $x > 5`
complexityGTP :: Parser PatternConstraint
complexityGTP = do
  void $ symbol "complexity"
  var <- varNameP
  void $ symbol ">"
  n <- lexeme L.decimal
  return $ ComplexityGT var (fromInteger n)

-- | Parse `$x matchesRegex "pattern"`
matchesRegexP :: Parser PatternConstraint
matchesRegexP = do
  var <- varNameP
  void $ symbol "matchesRegex"
  pat <- T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  return $ MatchesRegex var pat

-- | Parse `inModule "Foo.Bar"`
inModuleP :: Parser PatternConstraint
inModuleP = do
  void $ symbol "inModule"
  modName <- T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  return $ InModule modName

-- | Parse `notInModule "Foo.Bar"`
notInModuleP :: Parser PatternConstraint
notInModuleP = do
  void $ symbol "notInModule"
  modName <- T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  return $ NotInModule modName

-- | Parse `hasImport "Data.Text"`
hasImportP :: Parser PatternConstraint
hasImportP = do
  void $ symbol "hasImport"
  modName <- T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  return $ HasImport modName

-- | Parse `hasPragma "INLINE"`
hasPragmaP :: Parser PatternConstraint
hasPragmaP = do
  void $ symbol "hasPragma"
  pragma <- T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  return $ HasPragma pragma

-- | Parse `not constraint`
notConstraintP :: Parser PatternConstraint
notConstraintP = do
  void $ symbol "not"
  c <- constraintP
  return $ ConstraintNot c

-- | Parse `c1 || c2`
orConstraintP :: Parser PatternConstraint
orConstraintP = do
  c1 <- constraintP
  void $ symbol "||"
  c2 <- constraintP
  return $ ConstraintOr c1 c2

-- | Parse variable name (with or without $)
varNameP :: Parser Text
varNameP = lexeme $ do
  void $ optional $ char '$'
  T.pack <$> ((:) <$> lowerChar <*> many alphaNumChar)

--------------------------------------------------------------------------------
-- Pattern Matching
--------------------------------------------------------------------------------

-- | Result of pattern matching
data MatchResult = MatchResult
  { mrMatched :: Bool
  , mrBindings :: Map Text MatchBinding
  , mrConstraintResults :: Map Text Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A binding captured during matching
data MatchBinding = MatchBinding
  { mbText :: Text
      -- ^ The matched text
  , mbStartLine :: Int
  , mbStartCol :: Int
  , mbEndLine :: Int
  , mbEndCol :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Match an AST pattern against source text
matchASTPattern :: ASTPattern -> Text -> [MatchResult]
matchASTPattern astPat source =
  -- This is a simplified text-based matching for the core patterns
  -- Full AST matching would require GHC API integration
  let lines' = T.lines source
  in concatMap (matchLine astPat) $ zip [1..] lines'
  where
    matchLine :: ASTPattern -> (Int, Text) -> [MatchResult]
    matchLine pat (lineNum, lineText) =
      case pat of
        PVar name ->
          -- Metavariable matches any expression
          [ MatchResult True (Map.singleton name (mkBinding lineText lineNum 1)) Map.empty
          | not $ T.null $ T.strip lineText
          ]

        PWild ->
          [ MatchResult True Map.empty Map.empty ]

        PLit lit ->
          [ MatchResult True Map.empty Map.empty
          | matchesLiteral lit lineText
          ]

        PApp f x ->
          -- Match function application pattern
          matchApplication f x lineText lineNum

        PLam params body ->
          -- Match lambda pattern
          matchLambda params body lineText lineNum

        PIf cond t f ->
          -- Match if-then-else
          matchIfThenElse cond t f lineText lineNum

        PInfix l op r ->
          -- Match infix expression
          matchInfix l op r lineText lineNum

        PConstrained p constraints ->
          -- Match pattern with constraints
          let results = matchLine p (lineNum, lineText)
          in [ r { mrConstraintResults = Map.fromList [(showConstraint c, True) | c <- constraints] }
             | r <- results
             ]

        _ -> []  -- Other patterns require full AST

    mkBinding :: Text -> Int -> Int -> MatchBinding
    mkBinding txt ln col = MatchBinding
      { mbText = T.strip txt
      , mbStartLine = ln
      , mbStartCol = col
      , mbEndLine = ln
      , mbEndCol = col + T.length txt
      }

    matchesLiteral :: LiteralPattern -> Text -> Bool
    matchesLiteral lit txt = case lit of
      LitInt n -> T.pack (show n) `T.isInfixOf` txt
      LitFloat n -> T.pack (show n) `T.isInfixOf` txt
      LitChar c -> T.pack ['\'', c, '\''] `T.isInfixOf` txt
      LitString s -> ("\"" <> s <> "\"") `T.isInfixOf` txt
      LitBool b -> T.pack (show b) `T.isInfixOf` txt

    matchApplication :: ASTPattern -> ASTPattern -> Text -> Int -> [MatchResult]
    matchApplication _f _x lineText lineNum =
      -- Look for function application pattern: f x
      let stripped = T.strip lineText
          -- Try to find application patterns
          parts = T.words stripped
      in case parts of
           (firstPart : restParts@(_ : _)) ->
             [ MatchResult
                 { mrMatched = True
                 , mrBindings = Map.fromList
                     [ ("f", mkBinding firstPart lineNum 1)
                     , ("x", mkBinding (T.unwords restParts) lineNum (T.length firstPart + 2))
                     ]
                 , mrConstraintResults = Map.empty
                 }
             ]
           _ -> []

    matchLambda :: [Text] -> ASTPattern -> Text -> Int -> [MatchResult]
    matchLambda params _body lineText lineNum
      | "\\" `T.isInfixOf` lineText || "λ" `T.isInfixOf` lineText =
          -- Found a lambda
          let stripped = T.strip lineText
              -- Extract after \ or λ
              afterSlash = case T.breakOn "\\" stripped of
                (_, rest) | not (T.null rest) -> T.drop 1 rest
                _ -> case T.breakOn "λ" stripped of
                  (_, rest) | not (T.null rest) -> T.drop 1 rest
                  _ -> ""
              -- Split on ->
              (paramPart, bodyPart) = case T.breakOn "->" afterSlash of
                (p, b) | not (T.null b) -> (T.strip p, T.strip $ T.drop 2 b)
                _ -> ("", "")
              paramNames = T.words paramPart
          in [ MatchResult
                 { mrMatched = True
                 , mrBindings = Map.fromList $
                     [ (p, mkBinding n lineNum 1)
                     | (p, n) <- zip params paramNames
                     ] ++
                     [ ("body", mkBinding bodyPart lineNum (T.length stripped - T.length bodyPart))
                     ]
                 , mrConstraintResults = Map.empty
                 }
             | length paramNames >= length params
             ]
      | otherwise = []

    matchIfThenElse :: ASTPattern -> ASTPattern -> ASTPattern -> Text -> Int -> [MatchResult]
    matchIfThenElse _cond _t _f lineText lineNum
      | "if " `T.isInfixOf` lineText && " then " `T.isInfixOf` lineText =
          let stripped = T.strip lineText
              afterIf = snd $ T.breakOn "if " stripped
              (condPart, rest1) = T.breakOn " then " (T.drop 3 afterIf)
              afterThen = T.drop 6 rest1
              (thenPart, rest2) = T.breakOn " else " afterThen
              elsePart = T.drop 6 rest2
          in [ MatchResult
                 { mrMatched = True
                 , mrBindings = Map.fromList
                     [ ("cond", mkBinding (T.strip condPart) lineNum 1)
                     , ("t", mkBinding (T.strip thenPart) lineNum 1)
                     , ("f", mkBinding (T.strip elsePart) lineNum 1)
                     ]
                 , mrConstraintResults = Map.empty
                 }
             | not (T.null condPart) && not (T.null thenPart)
             ]
      | otherwise = []

    matchInfix :: ASTPattern -> Text -> ASTPattern -> Text -> Int -> [MatchResult]
    matchInfix _l op _r lineText lineNum =
      let stripped = T.strip lineText
          opText = " " <> op <> " "
      in if opText `T.isInfixOf` stripped
         then let (leftPart, rest) = T.breakOn opText stripped
                  rightPart = T.drop (T.length opText) rest
              in [ MatchResult
                     { mrMatched = True
                     , mrBindings = Map.fromList
                         [ ("l", mkBinding (T.strip leftPart) lineNum 1)
                         , ("r", mkBinding (T.strip rightPart) lineNum 1)
                         ]
                     , mrConstraintResults = Map.empty
                     }
                 ]
         else []

    showConstraint :: PatternConstraint -> Text
    showConstraint c = case c of
      NotFreeIn v e -> v <> " notFreeIn " <> e
      FreeIn v e -> v <> " freeIn " <> e
      HasType v _ -> v <> " :: ..."
      HasTypeClass v cls -> v <> " hasClass " <> cls
      NotEqual v1 v2 -> v1 <> " /= " <> v2
      IsAtomic v -> "isAtomic " <> v
      IsPure v -> "isPure " <> v
      IsMonadic v -> "isMonadic " <> v
      ComplexityLT v n -> "complexity " <> v <> " < " <> T.pack (show n)
      ComplexityGT v n -> "complexity " <> v <> " > " <> T.pack (show n)
      _ -> "<constraint>"

--------------------------------------------------------------------------------
-- Pattern Utilities
--------------------------------------------------------------------------------

-- | Get all metavariables in a pattern
patternVars :: ASTPattern -> Set Text
patternVars = \case
  PVar name -> Set.singleton name
  PWild -> Set.empty
  PLit _ -> Set.empty
  PApp f x -> patternVars f `Set.union` patternVars x
  PLam _ body -> patternVars body
  PLet bindings body ->
    Set.unions $ patternVars body : map (patternVars . snd) bindings
  PIf c t f -> patternVars c `Set.union` patternVars t `Set.union` patternVars f
  PCase scrut alts ->
    Set.unions $ patternVars scrut : concatMap (\(p, e) -> [patternVars p, patternVars e]) alts
  PTuple ps -> Set.unions $ map patternVars ps
  PList ps -> Set.unions $ map patternVars ps
  PCons h t -> patternVars h `Set.union` patternVars t
  PInfix l _ r -> patternVars l `Set.union` patternVars r
  PSection ml _ mr -> Set.unions $ mapMaybe (fmap patternVars) [ml, mr]
  PDo stmts -> Set.unions $ map doStmtVars stmts
  PTypeSig e _ -> patternVars e
  PAsPat _ p -> patternVars p
  PBang p -> patternVars p
  PView _ p -> patternVars p
  PGuarded gs -> Set.unions $ concatMap (\(g, e) -> [patternVars g, patternVars e]) gs
  PConstrained p _ -> patternVars p
  where
    doStmtVars (DoExpr e) = patternVars e
    doStmtVars (DoBind _ e) = patternVars e
    doStmtVars (DoLet bs) = Set.unions $ map (patternVars . snd) bs

-- | Calculate pattern complexity
patternComplexity :: ASTPattern -> Int
patternComplexity = \case
  PVar _ -> 1
  PWild -> 0
  PLit _ -> 1
  PApp f x -> 1 + patternComplexity f + patternComplexity x
  PLam ps body -> length ps + patternComplexity body
  PLet bindings body -> 2 + sum (map (patternComplexity . snd) bindings) + patternComplexity body
  PIf c t f -> 3 + patternComplexity c + patternComplexity t + patternComplexity f
  PCase scrut alts -> 2 + patternComplexity scrut + sum (map altComplexity alts)
  PTuple ps -> sum $ map patternComplexity ps
  PList ps -> sum $ map patternComplexity ps
  PCons h t -> 1 + patternComplexity h + patternComplexity t
  PInfix l _ r -> 1 + patternComplexity l + patternComplexity r
  PSection ml _ mr -> 1 + sum (map patternComplexity $ catMaybes [ml, mr])
  PDo stmts -> 1 + sum (map doStmtComplexity stmts)
  PTypeSig e _ -> 1 + patternComplexity e
  PAsPat _ p -> 1 + patternComplexity p
  PBang p -> patternComplexity p
  PView e p -> patternComplexity e + patternComplexity p
  PGuarded gs -> sum $ map (\(g, e) -> patternComplexity g + patternComplexity e) gs
  PConstrained p cs -> patternComplexity p + length cs
  where
    altComplexity (p, e) = patternComplexity p + patternComplexity e
    doStmtComplexity (DoExpr e) = patternComplexity e
    doStmtComplexity (DoBind _ e) = 1 + patternComplexity e
    doStmtComplexity (DoLet bs) = 1 + sum (map (patternComplexity . snd) bs)

-- | Simplify a pattern (remove redundant constructs)
simplifyPattern :: ASTPattern -> ASTPattern
simplifyPattern = \case
  PApp f PWild -> simplifyPattern f
  PApp PWild x -> simplifyPattern x
  PTuple [p] -> simplifyPattern p
  PList [p] -> PList [simplifyPattern p]
  PConstrained p [] -> simplifyPattern p
  PConstrained (PConstrained p cs1) cs2 -> PConstrained (simplifyPattern p) (cs1 ++ cs2)
  PApp f x -> PApp (simplifyPattern f) (simplifyPattern x)
  PLam ps body -> PLam ps (simplifyPattern body)
  PLet bs body -> PLet (map (fmap simplifyPattern) bs) (simplifyPattern body)
  PIf c t f -> PIf (simplifyPattern c) (simplifyPattern t) (simplifyPattern f)
  PCase s alts -> PCase (simplifyPattern s) (map (bimap simplifyPattern simplifyPattern) alts)
  PTuple ps -> PTuple (map simplifyPattern ps)
  PList ps -> PList (map simplifyPattern ps)
  PCons h t -> PCons (simplifyPattern h) (simplifyPattern t)
  PInfix l op r -> PInfix (simplifyPattern l) op (simplifyPattern r)
  p -> p
  where
    bimap f g (a, b) = (f a, g b)

-- | Normalize a pattern for comparison
normalizePattern :: ASTPattern -> ASTPattern
normalizePattern = simplifyPattern . sortConstraints
  where
    sortConstraints (PConstrained p cs) =
      PConstrained (normalizePattern p) (sortOn constraintKey cs)
    sortConstraints p = p

    constraintKey :: PatternConstraint -> Int
    constraintKey = \case
      NotFreeIn {} -> 1
      FreeIn {} -> 2
      HasType {} -> 3
      HasTypeClass {} -> 4
      NotEqual {} -> 5
      IsAtomic {} -> 6
      IsPure {} -> 7
      IsMonadic {} -> 8
      ComplexityLT {} -> 9
      ComplexityGT {} -> 10
      _ -> 100

--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

-- | Pretty print a pattern
ppPattern :: ASTPattern -> Text
ppPattern = \case
  PVar name -> "$" <> name
  PWild -> "_"
  PLit lit -> ppLiteral lit
  PApp f x -> ppPattern f <> " " <> ppAtom x
  PLam ps body -> "\\" <> T.unwords ps <> " -> " <> ppPattern body
  PLet bindings body ->
    "let " <> T.intercalate "; " (map ppBinding bindings) <> " in " <> ppPattern body
  PIf c t f -> "if " <> ppPattern c <> " then " <> ppPattern t <> " else " <> ppPattern f
  PCase scrut alts ->
    "case " <> ppPattern scrut <> " of { " <>
    T.intercalate "; " (map ppAlt alts) <> " }"
  PTuple ps -> "(" <> T.intercalate ", " (map ppPattern ps) <> ")"
  PList ps -> "[" <> T.intercalate ", " (map ppPattern ps) <> "]"
  PCons h t -> ppPattern h <> " : " <> ppPattern t
  PInfix l op r -> ppPattern l <> " " <> op <> " " <> ppPattern r
  PSection ml op mr ->
    "(" <> maybe "" ppPattern ml <> " " <> op <> " " <> maybe "" ppPattern mr <> ")"
  PDo stmts -> "do { " <> T.intercalate "; " (map ppDoStmt stmts) <> " }"
  PTypeSig e typ -> ppPattern e <> " :: " <> ppType typ
  PAsPat name p -> name <> "@" <> ppAtom p
  PBang p -> "!" <> ppAtom p
  PView e p -> ppPattern e <> " -> " <> ppPattern p
  PGuarded gs -> T.intercalate " | " $ map ppGuard gs
  PConstrained p cs -> ppPattern p <> " where " <> T.intercalate " && " (map ppConstraint cs)
  where
    ppAtom p@(PVar _) = ppPattern p
    ppAtom p@(PLit _) = ppPattern p
    ppAtom p@PWild = ppPattern p
    ppAtom p = "(" <> ppPattern p <> ")"

    ppBinding (name, expr) = name <> " = " <> ppPattern expr
    ppAlt (pat, expr) = ppPattern pat <> " -> " <> ppPattern expr
    ppDoStmt (DoExpr e) = ppPattern e
    ppDoStmt (DoBind name e) = name <> " <- " <> ppPattern e
    ppDoStmt (DoLet bs) = "let " <> T.intercalate "; " (map ppBinding bs)
    ppGuard (g, e) = ppPattern g <> " -> " <> ppPattern e

    ppLiteral (LitInt n) = T.pack $ show n
    ppLiteral (LitFloat n) = T.pack $ show n
    ppLiteral (LitChar c) = T.pack ['\'', c, '\'']
    ppLiteral (LitString s) = "\"" <> s <> "\""
    ppLiteral (LitBool b) = T.pack $ show b

    ppType (TVar name) = name
    ppType (TCon name) = name
    ppType (TApp t1 t2) = ppType t1 <> " " <> ppTypeAtom t2
    ppType (TFun t1 t2) = ppTypeAtom t1 <> " -> " <> ppType t2
    ppType (TTuple ts) = "(" <> T.intercalate ", " (map ppType ts) <> ")"
    ppType (TList t) = "[" <> ppType t <> "]"
    ppType (TForall vs t) = "forall " <> T.unwords vs <> ". " <> ppType t
    ppType (TConstraint cs t) = "(" <> T.intercalate ", " cs <> ") => " <> ppType t

    ppTypeAtom t@(TVar _) = ppType t
    ppTypeAtom t@(TCon _) = ppType t
    ppTypeAtom t = "(" <> ppType t <> ")"

-- | Pretty print a constraint
ppConstraint :: PatternConstraint -> Text
ppConstraint = \case
  NotFreeIn v e -> "$" <> v <> " `notFreeIn` $" <> e
  FreeIn v e -> "$" <> v <> " `freeIn` $" <> e
  HasType v typ -> "$" <> v <> " :: " <> ppType' typ
  HasTypeClass v cls -> "$" <> v <> " hasClass " <> cls
  NotEqual v1 v2 -> "$" <> v1 <> " /= $" <> v2
  IsAtomic v -> "isAtomic $" <> v
  IsPure v -> "isPure $" <> v
  IsMonadic v -> "isMonadic $" <> v
  ComplexityLT v n -> "complexity $" <> v <> " < " <> T.pack (show n)
  ComplexityGT v n -> "complexity $" <> v <> " > " <> T.pack (show n)
  MatchesRegex v pat -> "$" <> v <> " matchesRegex \"" <> pat <> "\""
  InModule m -> "inModule \"" <> m <> "\""
  NotInModule m -> "notInModule \"" <> m <> "\""
  HasPragma p -> "hasPragma \"" <> p <> "\""
  HasImport i -> "hasImport \"" <> i <> "\""
  ConstraintAnd c1 c2 -> ppConstraint c1 <> " && " <> ppConstraint c2
  ConstraintOr c1 c2 -> "(" <> ppConstraint c1 <> " || " <> ppConstraint c2 <> ")"
  ConstraintNot c -> "not " <> ppConstraint c
  where
    ppType' (TVar name) = name
    ppType' (TCon name) = name
    ppType' _t = "..."
