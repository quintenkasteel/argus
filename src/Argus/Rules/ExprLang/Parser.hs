{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Rules.ExprLang.Parser
-- Description : Parser for the Argus expression language
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides parsing for Argus's typed expression language.
-- The language supports a subset of Haskell-like syntax with built-in
-- functions for code analysis.
--
-- Example expressions:
--
-- @
-- length(matchedText) > 10
-- isPure($body) && not(freeIn($x, $body))
-- complexity($expr) < 15 && hasType($f, "Int -> Int")
-- @
module Argus.Rules.ExprLang.Parser
  ( -- * Parsing
    parseExpr
  , parseValue
  , parseType
  , ParseResult (..)
  , ParseError (..)

    -- * Tokenization
  , tokenize
  , Token (..)
  , TokenType (..)
  ) where

import Control.Monad (guard, void)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (ParseError, Token, token)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Argus.Rules.ExprLang.Types

--------------------------------------------------------------------------------
-- Parse Result
--------------------------------------------------------------------------------

-- | Result of parsing
data ParseResult a
  = ParseOk a
  | ParseErr ParseError
  deriving stock (Eq, Show)

instance Functor ParseResult where
  fmap f (ParseOk x) = ParseOk (f x)
  fmap _ (ParseErr e) = ParseErr e

instance Applicative ParseResult where
  pure = ParseOk
  ParseOk f <*> ParseOk x = ParseOk (f x)
  ParseErr e <*> _ = ParseErr e
  _ <*> ParseErr e = ParseErr e

instance Monad ParseResult where
  ParseOk x >>= f = f x
  ParseErr e >>= _ = ParseErr e

-- | Parse error
data ParseError = ParseError
  { peMessage :: Text
  , peLine :: Int
  , peColumn :: Int
  , peInput :: Text
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Parser Type
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------
-- Main Parsing Functions
--------------------------------------------------------------------------------

-- | Parse an expression
parseExpr :: Text -> ParseResult Expr
parseExpr input =
  case M.parse (spaceConsumer *> exprP <* eof) "" input of
    Left err -> ParseErr $ ParseError
      { peMessage = T.pack $ M.errorBundlePretty err
      , peLine = getLine err
      , peColumn = getCol err
      , peInput = input
      }
    Right expr -> ParseOk expr
  where
    getLine err = M.unPos $ M.sourceLine $ M.pstateSourcePos $ M.bundlePosState err
    getCol err = M.unPos $ M.sourceColumn $ M.pstateSourcePos $ M.bundlePosState err

-- | Parse a value literal
parseValue :: Text -> ParseResult Value
parseValue input =
  case M.parse (spaceConsumer *> valueP <* eof) "" input of
    Left err -> ParseErr $ ParseError
      { peMessage = T.pack $ M.errorBundlePretty err
      , peLine = 1
      , peColumn = 1
      , peInput = input
      }
    Right val -> ParseOk val

-- | Parse a type
parseType :: Text -> ParseResult ExprType
parseType input =
  case M.parse (spaceConsumer *> typeP <* eof) "" input of
    Left err -> ParseErr $ ParseError
      { peMessage = T.pack $ M.errorBundlePretty err
      , peLine = 1
      , peColumn = 1
      , peInput = input
      }
    Right typ -> ParseOk typ

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

-- | Whitespace consumer
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

-- | Parse identifier
identifier :: Parser Text
identifier = lexeme $ do
  first <- lowerChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  let name = T.pack (first : rest)
  guard (name `notElem` reservedWords)
  return name

-- | Parse constructor (starts with uppercase)
constructor :: Parser Text
constructor = lexeme $ do
  first <- upperChar
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  return $ T.pack (first : rest)

-- | Reserved words
reservedWords :: [Text]
reservedWords =
  [ "if", "then", "else", "let", "in", "case", "of"
  , "true", "false", "True", "False"
  , "and", "or", "not"
  ]

-- | Parse reserved word
reserved :: Text -> Parser ()
reserved w = lexeme $ do
  void $ string w
  notFollowedBy alphaNumChar

--------------------------------------------------------------------------------
-- Expression Parser
--------------------------------------------------------------------------------

-- | Parse expression (handles operator precedence)
exprP :: Parser Expr
exprP = orExprP

-- | Parse or expression (lowest precedence)
orExprP :: Parser Expr
orExprP = do
  left <- andExprP
  rest <- many $ do
    void $ symbol "||" <|> (reserved "or" >> return "or")
    andExprP
  return $ foldl' (EOp OpOr) left rest

-- | Parse and expression
andExprP :: Parser Expr
andExprP = do
  left <- compExprP
  rest <- many $ do
    void $ symbol "&&" <|> (reserved "and" >> return "and")
    compExprP
  return $ foldl' (EOp OpAnd) left rest

-- | Parse comparison expression
compExprP :: Parser Expr
compExprP = do
  left <- addExprP
  mRest <- optional $ do
    op <- compOpP
    right <- addExprP
    return (op, right)
  return $ case mRest of
    Nothing -> left
    Just (op, right) -> EOp op left right

-- | Parse comparison operator
compOpP :: Parser BinOp
compOpP = choice
  [ OpEq <$ symbol "=="
  , OpNeq <$ (symbol "!=" <|> symbol "/=")
  , OpLte <$ symbol "<="
  , OpGte <$ symbol ">="
  , OpLt <$ symbol "<"
  , OpGt <$ symbol ">"
  , OpIn <$ (reserved "in" >> return "in")
  ]

-- | Parse additive expression
addExprP :: Parser Expr
addExprP = do
  left <- mulExprP
  rest <- many addOpP
  return $ foldl' (\l (o, r) -> EOp o l r) left rest
  where
    addOpP :: Parser (BinOp, Expr)
    addOpP = do
      op <- choice
        [ OpAdd <$ symbol "+"
        , OpSub <$ symbol "-"
        , OpConcat <$ symbol "++"
        ]
      e <- mulExprP
      return (op, e)

-- | Parse multiplicative expression
mulExprP :: Parser Expr
mulExprP = do
  left <- unaryExprP
  rest <- many mulOpP
  return $ foldl' (\l (o, r) -> EOp o l r) left rest
  where
    mulOpP :: Parser (BinOp, Expr)
    mulOpP = do
      op <- choice
        [ OpMul <$ symbol "*"
        , OpDiv <$ symbol "/"
        , OpMod <$ symbol "%"
        ]
      e <- unaryExprP
      return (op, e)

-- | Parse unary expression
unaryExprP :: Parser Expr
unaryExprP = choice
  [ ENot <$> (reserved "not" *> unaryExprP)
  , ENot <$> (symbol "!" *> unaryExprP)
  , postfixExprP
  ]

-- | Parse postfix expression (function calls, field access, indexing)
postfixExprP :: Parser Expr
postfixExprP = do
  base <- atomP
  postfixes <- many postfixP
  return $ foldl' applyPostfix base postfixes
  where
    postfixP :: Parser (Expr -> Expr)
    postfixP = choice
      [ do  -- Function call
          args <- between (symbol "(") (symbol ")") (exprP `sepBy` symbol ",")
          return $ \f -> EApp f args
      , do  -- Field access
          void $ symbol "."
          field <- identifier
          return $ \e -> EField e field
      , do  -- Index access
          idx <- between (symbol "[") (symbol "]") exprP
          return $ \e -> EIndex e idx
      ]

    applyPostfix e f = f e

-- | Parse atomic expression
atomP :: Parser Expr
atomP = choice
  [ try lambdaP
  , try letP
  , try ifP
  , try caseP
  , try metavarP
  , try matchP
  , try builtinP
  , try literalP
  , try listP
  , try recordP
  , try varP
  , between (symbol "(") (symbol ")") exprP
  ]

-- | Parse lambda expression
lambdaP :: Parser Expr
lambdaP = do
  void $ symbol "\\" <|> symbol "λ"
  params <- some identifier
  void $ symbol "->"
  body <- exprP
  return $ ELam params body

-- | Parse let expression
letP :: Parser Expr
letP = do
  reserved "let"
  bindings <- binding `sepBy1` symbol ";"
  reserved "in"
  body <- exprP
  return $ ELet bindings body
  where
    binding = do
      name <- identifier
      void $ symbol "="
      expr <- exprP
      return (name, expr)

-- | Parse if expression
ifP :: Parser Expr
ifP = do
  reserved "if"
  cond <- exprP
  reserved "then"
  thenE <- exprP
  reserved "else"
  elseE <- exprP
  return $ EIf cond thenE elseE

-- | Parse case expression
caseP :: Parser Expr
caseP = do
  reserved "case"
  scrut <- exprP
  reserved "of"
  -- Simplified: just parse first alt
  _ <- many $ do
    pat <- exprP
    void $ symbol "->"
    body <- exprP
    optional $ symbol ";"
    return (pat, body)
  return scrut  -- Simplified for now

-- | Parse metavariable ($x)
metavarP :: Parser Expr
metavarP = do
  void $ char '$'
  name <- T.pack <$> some (alphaNumChar <|> char '_')
  spaceConsumer
  return $ EMetavar name

-- | Parse $match
matchP :: Parser Expr
matchP = EMatch <$> (symbol "$match" *> pure (ELit (VString "")))

-- | Parse built-in function
builtinP :: Parser Expr
builtinP = EBuiltin <$> choice
  -- Text operations
  [ BLength <$ reserved "length"
  , BConcat <$ reserved "concat"
  , BToUpper <$ reserved "toUpper"
  , BToLower <$ reserved "toLower"
  , BStrip <$ reserved "strip"
  , BSplit <$ reserved "split"
  , BContains <$ reserved "contains"
  , BStartsWith <$ reserved "startsWith"
  , BEndsWith <$ reserved "endsWith"
  , BReplace <$ reserved "replace"
  , BRegexMatch <$ reserved "regexMatch"
  , BRegexFind <$ reserved "regexFind"

  -- List operations
  , BHead <$ reserved "head"
  , BTail <$ reserved "tail"
  , BInit <$ reserved "init"
  , BLast <$ reserved "last"
  , BTake <$ reserved "take"
  , BDrop <$ reserved "drop"
  , BReverse <$ reserved "reverse"
  , BSort <$ reserved "sort"
  , BNub <$ reserved "nub"
  , BFilter <$ reserved "filter"
  , BMap <$ reserved "map"
  , BFoldl <$ reserved "foldl"
  , BFoldr <$ reserved "foldr"
  , BAll <$ reserved "all"
  , BAny <$ reserved "any"
  , BElem <$ reserved "elem"
  , BNotElem <$ reserved "notElem"
  , BZip <$ reserved "zip"
  , BZipWith <$ reserved "zipWith"
  , BFlatten <$ reserved "flatten"

  -- Set operations
  , BUnion <$ reserved "union"
  , BIntersect <$ reserved "intersect"
  , BDifference <$ reserved "difference"
  , BIsSubset <$ reserved "isSubset"
  , BFromList <$ reserved "fromList"
  , BToList <$ reserved "toList"

  -- Numeric operations
  , BAbs <$ reserved "abs"
  , BMax <$ reserved "max"
  , BMin <$ reserved "min"
  , BSum <$ reserved "sum"
  , BProduct <$ reserved "product"
  , BCeiling <$ reserved "ceiling"
  , BFloor <$ reserved "floor"
  , BRound <$ reserved "round"

  -- Type queries
  , BTypeOf <$ reserved "typeOf"
  , BKindOf <$ reserved "kindOf"
  , BHasType <$ reserved "hasType"
  , BHasTypeClass <$ reserved "hasTypeClass"
  , BIsFunctor <$ reserved "isFunctor"
  , BIsMonad <$ reserved "isMonad"
  , BIsApplicative <$ reserved "isApplicative"

  -- Purity queries
  , BIsPure <$ reserved "isPure"
  , BHasEffect <$ reserved "hasEffect"
  , BEffectType <$ reserved "effectType"

  -- AST queries
  , BComplexity <$ reserved "complexity"
  , BFreeVars <$ reserved "freeVars"
  , BBindings <$ reserved "bindings"
  , BIsLiteral <$ reserved "isLiteral"
  , BIsVariable <$ reserved "isVariable"
  , BIsApplication <$ reserved "isApplication"
  , BIsLambda <$ reserved "isLambda"
  , BIsAtomic <$ reserved "isAtomic"
  , BArity <$ reserved "arity"

  -- Context queries
  , BModuleName <$ reserved "moduleName"
  , BFilePath <$ reserved "filePath"
  , BLineNumber <$ reserved "lineNumber"
  , BColumnNumber <$ reserved "columnNumber"
  , BMatchedText <$ reserved "matchedText"
  , BMetavar <$ reserved "metavar"
  , BImports <$ reserved "imports"
  , BExports <$ reserved "exports"
  , BPragmas <$ reserved "pragmas"
  , BHasImport <$ reserved "hasImport"
  , BHasPragma <$ reserved "hasPragma"

  -- Utilities
  , BShow <$ reserved "show"
  , BRead <$ reserved "read"
  , BError <$ reserved "error"
  , BTrace <$ reserved "trace"
  ]

-- | Parse literal
literalP :: Parser Expr
literalP = ELit <$> valueP

-- | Parse value
valueP :: Parser Value
valueP = choice
  [ try $ VFloat <$> lexeme L.float
  , try $ VInt <$> lexeme L.decimal
  , VBool True <$ (reserved "true" <|> reserved "True")
  , VBool False <$ (reserved "false" <|> reserved "False")
  , VString <$> stringLitP
  , VNothing <$ reserved "Nothing"
  ]

-- | Parse string literal
stringLitP :: Parser Text
stringLitP = lexeme $ do
  void $ char '"'
  content <- many stringChar
  void $ char '"'
  return $ T.pack content
  where
    stringChar = choice
      [ char '\\' *> escapeChar
      , noneOf ("\"" :: String)
      ]
    escapeChar = choice
      [ '"' <$ char '"'
      , '\\' <$ char '\\'
      , '\n' <$ char 'n'
      , '\r' <$ char 'r'
      , '\t' <$ char 't'
      ]

-- | Parse list
listP :: Parser Expr
listP = EList <$> between (symbol "[") (symbol "]") (exprP `sepBy` symbol ",")

-- | Parse record
recordP :: Parser Expr
recordP = ERecord <$> between (symbol "{") (symbol "}") (field `sepBy` symbol ",")
  where
    field = do
      name <- identifier
      void $ symbol "=" <|> symbol ":"
      expr <- exprP
      return (name, expr)

-- | Parse variable
varP :: Parser Expr
varP = EVar <$> identifier

--------------------------------------------------------------------------------
-- Type Parser
--------------------------------------------------------------------------------

-- | Parse type
typeP :: Parser ExprType
typeP = funcTypeP

-- | Parse function type
funcTypeP :: Parser ExprType
funcTypeP = do
  t <- atomTypeP
  mRest <- optional $ do
    void $ symbol "->"
    funcTypeP
  return $ case mRest of
    Nothing -> t
    Just rest -> TFunc [t] rest

-- | Parse atomic type
atomTypeP :: Parser ExprType
atomTypeP = choice
  [ TBool <$ reserved "Bool"
  , TInt <$ reserved "Int"
  , TFloat <$ reserved "Float"
  , TString <$ reserved "String"
  , TAny <$ reserved "Any"
  , try listTypeP
  , try setTypeP
  , try maybeTypeP
  , try tupleTypeP
  , TVar <$> identifier
  , between (symbol "(") (symbol ")") typeP
  ]

-- | Parse list type
listTypeP :: Parser ExprType
listTypeP = TList <$> between (symbol "[") (symbol "]") typeP

-- | Parse set type
setTypeP :: Parser ExprType
setTypeP = do
  reserved "Set"
  TSet <$> atomTypeP

-- | Parse maybe type
maybeTypeP :: Parser ExprType
maybeTypeP = do
  reserved "Maybe"
  TMaybe <$> atomTypeP

-- | Parse tuple type (as record with numbered fields)
tupleTypeP :: Parser ExprType
tupleTypeP = do
  void $ symbol "("
  first <- typeP
  rest <- many $ symbol "," *> typeP
  void $ symbol ")"
  case rest of
    [] -> return first
    _ -> return $ TRecord $ Map.fromList $ zip (map (T.pack . show) [0..]) (first : rest)

--------------------------------------------------------------------------------
-- Tokenization
--------------------------------------------------------------------------------

-- | Token type
data Token = Token
  { tokType :: TokenType
  , tokText :: Text
  , tokLine :: Int
  , tokColumn :: Int
  }
  deriving stock (Eq, Show)

-- | Token types
data TokenType
  = TokIdent
  | TokConstr
  | TokInt
  | TokFloat
  | TokString
  | TokOp
  | TokParen
  | TokBracket
  | TokBrace
  | TokComma
  | TokDot
  | TokColon
  | TokArrow
  | TokLambda
  | TokMetavar
  | TokKeyword
  | TokEOF
  deriving stock (Eq, Show, Ord, Bounded, Enum)

-- | Tokenize input
tokenize :: Text -> [Token]
tokenize input = go 1 1 (T.unpack input)
  where
    go :: Int -> Int -> String -> [Token]
    go _ _ [] = []
    go ln col (c:cs)
      | isSpace c =
          if c == '\n'
          then go (ln + 1) 1 cs
          else go ln (col + 1) cs
      | c == '-' && take 1 cs == "-" =
          let (_, rest) = span (/= '\n') cs
          in go ln col rest
      | c == '$' =
          let (name, rest) = span isIdChar cs
          in Token TokMetavar (T.pack ('$' : name)) ln col : go ln (col + length name + 1) rest
      | isLower c || c == '_' =
          let (rest', rem') = span isIdChar cs
              txt = T.pack (c : rest')
              typ = if txt `elem` reservedWords then TokKeyword else TokIdent
          in Token typ txt ln col : go ln (col + T.length txt) rem'
      | isUpper c =
          let (rest', rem') = span isIdChar cs
          in Token TokConstr (T.pack (c : rest')) ln col : go ln (col + length rest' + 1) rem'
      | isDigit c =
          let (num, rest') = span (\x -> isDigit x || x == '.') (c:cs)
              typ = if '.' `elem` num then TokFloat else TokInt
          in Token typ (T.pack num) ln col : go ln (col + length num) rest'
      | c == '"' =
          let (str, rest') = parseString cs
          in Token TokString (T.pack str) ln col : go ln (col + length str + 2) rest'
      | c `elem` ("()[]{}," :: String) =
          let typ = case c of
                '(' -> TokParen; ')' -> TokParen
                '[' -> TokBracket; ']' -> TokBracket
                '{' -> TokBrace; '}' -> TokBrace
                ',' -> TokComma
                _ -> TokOp
          in Token typ (T.singleton c) ln col : go ln (col + 1) cs
      | c == '.' =
          Token TokDot "." ln col : go ln (col + 1) cs
      | c == ':' =
          if take 1 cs == ":"
          then Token TokOp "::" ln col : go ln (col + 2) (drop 1 cs)
          else Token TokColon ":" ln col : go ln (col + 1) cs
      | c == '-' && take 1 cs == ">" =
          Token TokArrow "->" ln col : go ln (col + 2) (drop 1 cs)
      | c == '\\' || c == 'λ' =
          Token TokLambda (T.singleton c) ln col : go ln (col + 1) cs
      | c `elem` ("+-*/%<>=!&|" :: String) =
          let (op, rest') = span (`elem` ("+-*/%<>=!&|" :: String)) (c:cs)
          in Token TokOp (T.pack op) ln col : go ln (col + length op) rest'
      | otherwise =
          go ln (col + 1) cs

    isIdChar c = isAlphaNum c || c == '_' || c == '\''

    parseString :: String -> (String, String)
    parseString [] = ([], [])
    parseString ('"':rest) = ([], rest)
    parseString ('\\':c:rest) =
      let (s, r) = parseString rest
      in (c:s, r)
    parseString (c:rest) =
      let (s, r) = parseString rest
      in (c:s, r)
