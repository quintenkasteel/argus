{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : ASTMatchSpec
-- Description : Tests for AST-based pattern matching
--
-- Comprehensive test suite for the AST pattern matching engine
-- that ensures correct behavior for various pattern types.
module ASTMatchSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set qualified as Set
import Data.Maybe (isJust, isNothing)

import Argus.Rules.ASTMatch

-- Helper to check if lookup succeeds
hasBinding :: Text -> Subst -> Bool
hasBinding var subst = isJust (lookupSubst var subst)

-- Helper to parse and get Right result (for tests)
parseExpr :: Text -> LHsExpr GhcPs
parseExpr txt = case parsePatternPure txt of
  Right e -> e
  Left _ -> error $ "Failed to parse: " <> T.unpack txt

spec :: Spec
spec = describe "Argus.Rules.ASTMatch" $ do
  describe "Pattern Parsing" $ do
    describe "parsePatternPure" $ do
      it "parses simple variable" $ do
        case parsePatternPure "x" of
          Right _ -> True `shouldBe` True
          Left err -> fail $ T.unpack err

      it "parses function application" $ do
        case parsePatternPure "f x" of
          Right _ -> True `shouldBe` True
          Left err -> fail $ T.unpack err

      it "parses nested application" $ do
        case parsePatternPure "map f (map g x)" of
          Right _ -> True `shouldBe` True
          Left err -> fail $ T.unpack err

      it "parses if-then-else" $ do
        case parsePatternPure "if x then True else False" of
          Right _ -> True `shouldBe` True
          Left err -> fail $ T.unpack err

      it "parses lambda expression" $ do
        case parsePatternPure "\\x -> f x" of
          Right _ -> True `shouldBe` True
          Left err -> fail $ T.unpack err

      it "parses infix operator" $ do
        case parsePatternPure "x + y" of
          Right _ -> True `shouldBe` True
          Left err -> fail $ T.unpack err

      it "parses list literal" $ do
        case parsePatternPure "[x, y, z]" of
          Right _ -> True `shouldBe` True
          Left err -> fail $ T.unpack err

      it "parses tuple" $ do
        case parsePatternPure "(x, y)" of
          Right _ -> True `shouldBe` True
          Left err -> fail $ T.unpack err

      it "fails on invalid syntax" $ do
        case parsePatternPure "let in" of
          Left _ -> True `shouldBe` True
          Right _ -> fail "Should have failed"

  describe "isUnifyVar" $ do
    it "single lowercase letter is unification var" $ do
      isUnifyVar "x" `shouldBe` True
      isUnifyVar "f" `shouldBe` True
      isUnifyVar "a" `shouldBe` True

    it "single uppercase letter is NOT unification var" $ do
      isUnifyVar "X" `shouldBe` False
      isUnifyVar "F" `shouldBe` False

    it "underscore prefix is unification var" $ do
      isUnifyVar "_x" `shouldBe` True
      isUnifyVar "_foo" `shouldBe` True
      isUnifyVar "_" `shouldBe` True

    it "dollar prefix is unification var (Semgrep style)" $ do
      isUnifyVar "$x" `shouldBe` True
      isUnifyVar "$foo" `shouldBe` True

    it "multi-char names are NOT unification vars" $ do
      isUnifyVar "foo" `shouldBe` False
      isUnifyVar "bar" `shouldBe` False
      isUnifyVar "map" `shouldBe` False

  describe "isEllipsisVar" $ do
    it "triple underscore is ellipsis" $ do
      isEllipsisVar "___" `shouldBe` True

    it "Semgrep ellipsis is recognized" $ do
      isEllipsisVar "$..." `shouldBe` True

    it "regular vars are not ellipsis" $ do
      isEllipsisVar "x" `shouldBe` False
      isEllipsisVar "_x" `shouldBe` False

  describe "Expression Unification" $ do
    describe "simple patterns" $ do
      it "unifies variable with any expression" $ do
        let pat = parseExpr "x"
            target = parseExpr "foo"
        case unifyExpr pat target of
          Just subst -> hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "unifies same literal" $ do
        let pat = parseExpr "42"
            target = parseExpr "42"
        isJust (unifyExpr pat target) `shouldBe` True

      it "fails to unify different literals" $ do
        let pat = parseExpr "42"
            target = parseExpr "43"
        isNothing (unifyExpr pat target) `shouldBe` True

      it "unifies same function name" $ do
        let pat = parseExpr "map"
            target = parseExpr "map"
        isJust (unifyExpr pat target) `shouldBe` True

      it "fails to unify different function names" $ do
        let pat = parseExpr "map"
            target = parseExpr "filter"
        isNothing (unifyExpr pat target) `shouldBe` True

    describe "application patterns" $ do
      it "unifies f x with foo bar" $ do
        let pat = parseExpr "f x"
            target = parseExpr "foo bar"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "f" subst `shouldBe` True
            hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "unifies nested application" $ do
        -- Pattern: f (g x) = App f (App g x)
        -- Target: show (length xs) = App show (App length xs)
        let pat = parseExpr "f (g x)"
            target = parseExpr "show (length xs)"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "f" subst `shouldBe` True
            hasBinding "g" subst `shouldBe` True
            hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "captures concrete function names" $ do
        let pat = parseExpr "map f x"
            target = parseExpr "map show xs"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "f" subst `shouldBe` True
            hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "fails when function names don't match" $ do
        let (Right pat) = parsePatternPure "map f x"
            (Right target) = parsePatternPure "filter pred xs"
        isNothing (unifyExpr pat target) `shouldBe` True

    describe "if-then-else patterns" $ do
      it "unifies if pattern" $ do
        let (Right pat) = parsePatternPure "if c then t else e"
            (Right target) = parsePatternPure "if isValid x then process x else error \"invalid\""
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "c" subst `shouldBe` True
            hasBinding "t" subst `shouldBe` True
            hasBinding "e" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "fails when structure differs" $ do
        let (Right pat) = parsePatternPure "if c then t else e"
            (Right target) = parsePatternPure "case x of { True -> t; False -> e }"
        isNothing (unifyExpr pat target) `shouldBe` True

    describe "operator patterns" $ do
      it "unifies infix operator" $ do
        let (Right pat) = parsePatternPure "x + y"
            (Right target) = parsePatternPure "a + b"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "x" subst `shouldBe` True
            hasBinding "y" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "fails with different operators" $ do
        let (Right pat) = parsePatternPure "x + y"
            (Right target) = parsePatternPure "a - b"
        isNothing (unifyExpr pat target) `shouldBe` True

    describe "list patterns" $ do
      it "unifies list literals" $ do
        let (Right pat) = parsePatternPure "[x, y]"
            (Right target) = parsePatternPure "[1, 2]"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "x" subst `shouldBe` True
            hasBinding "y" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "fails with different list lengths" $ do
        let (Right pat) = parsePatternPure "[x, y]"
            (Right target) = parsePatternPure "[1, 2, 3]"
        isNothing (unifyExpr pat target) `shouldBe` True

    describe "tuple patterns" $ do
      it "unifies tuple" $ do
        let (Right pat) = parsePatternPure "(x, y)"
            (Right target) = parsePatternPure "(1, \"hello\")"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "x" subst `shouldBe` True
            hasBinding "y" subst `shouldBe` True
          Nothing -> fail "Should have unified"

    describe "parentheses handling" $ do
      it "sees through parentheses in target" $ do
        let (Right pat) = parsePatternPure "f x"
            (Right target) = parsePatternPure "(f) (x)"
        isJust (unifyExpr pat target) `shouldBe` True

  describe "Substitution Operations" $ do
    describe "mergeSubst" $ do
      it "merges disjoint substitutions" $ do
        let (Right e1) = parsePatternPure "foo"
            (Right e2) = parsePatternPure "bar"
            s1 = singletonSubst "x" e1
            s2 = singletonSubst "y" e2
        case mergeSubst s1 s2 of
          Just merged -> do
            hasBinding "x" merged `shouldBe` True
            hasBinding "y" merged `shouldBe` True
          Nothing -> fail "Should have merged"

      it "merges consistent overlapping substitutions" $ do
        let (Right e) = parsePatternPure "foo"
            s1 = singletonSubst "x" e
            s2 = singletonSubst "x" e
        isJust (mergeSubst s1 s2) `shouldBe` True

      it "fails on inconsistent substitutions" $ do
        let (Right e1) = parsePatternPure "foo"
            (Right e2) = parsePatternPure "bar"
            s1 = singletonSubst "x" e1
            s2 = singletonSubst "x" e2
        isNothing (mergeSubst s1 s2) `shouldBe` True

  describe "Side Conditions" $ do
    describe "isAtomicExpr" $ do
      it "variable is atomic" $ do
        let (Right e) = parsePatternPure "x"
        isAtomicExpr e `shouldBe` True

      it "literal is atomic" $ do
        let (Right e) = parsePatternPure "42"
        isAtomicExpr e `shouldBe` True

      it "application is not atomic" $ do
        let (Right e) = parsePatternPure "f x"
        isAtomicExpr e `shouldBe` False

    describe "isLiteralExpr" $ do
      it "number is literal" $ do
        let (Right e) = parsePatternPure "42"
        isLiteralExpr e `shouldBe` True

      it "string is literal" $ do
        let (Right e) = parsePatternPure "\"hello\""
        isLiteralExpr e `shouldBe` True

      it "variable is not literal" $ do
        let (Right e) = parsePatternPure "x"
        isLiteralExpr e `shouldBe` False

    describe "isVarExpr" $ do
      it "variable is var" $ do
        let (Right e) = parsePatternPure "x"
        isVarExpr e `shouldBe` True

      it "application is not var" $ do
        let (Right e) = parsePatternPure "f x"
        isVarExpr e `shouldBe` False

    describe "isFunExpr" $ do
      it "lambda is function" $ do
        let (Right e) = parsePatternPure "\\x -> x"
        isFunExpr e `shouldBe` True

      it "variable can be function" $ do
        let (Right e) = parsePatternPure "f"
        isFunExpr e `shouldBe` True

    describe "isConstructorExpr" $ do
      it "uppercase name is constructor" $ do
        let (Right e) = parsePatternPure "Just"
        isConstructorExpr e `shouldBe` True

      it "lowercase name is not constructor" $ do
        let (Right e) = parsePatternPure "foo"
        isConstructorExpr e `shouldBe` False

      it "list is constructor" $ do
        let (Right e) = parsePatternPure "[1, 2, 3]"
        isConstructorExpr e `shouldBe` True

      it "tuple is constructor" $ do
        let (Right e) = parsePatternPure "(1, 2)"
        isConstructorExpr e `shouldBe` True

    describe "evalSideCondition" $ do
      it "IsAtomic passes for atomic" $ do
        let (Right e) = parsePatternPure "42"
            subst = singletonSubst "x" e
        evalSideCondition subst (IsAtomic "x") `shouldBe` True

      it "IsAtomic fails for non-atomic" $ do
        let (Right e) = parsePatternPure "f x"
            subst = singletonSubst "x" e
        evalSideCondition subst (IsAtomic "x") `shouldBe` False

      it "NotEqual passes for different" $ do
        let (Right e1) = parsePatternPure "foo"
            (Right e2) = parsePatternPure "bar"
            subst = singletonSubst "x" e1 <> singletonSubst "y" e2
        evalSideCondition subst (NotEqual "x" "y") `shouldBe` True

      it "NotEqual fails for same" $ do
        let (Right e) = parsePatternPure "foo"
            subst = singletonSubst "x" e <> singletonSubst "y" e
        evalSideCondition subst (NotEqual "x" "y") `shouldBe` False

      it "Always is always true" $ do
        evalSideCondition emptySubst Always `shouldBe` True

      it "Never is always false" $ do
        evalSideCondition emptySubst Never `shouldBe` False

      it "And combines conditions" $ do
        evalSideCondition emptySubst (And [Always, Always]) `shouldBe` True
        evalSideCondition emptySubst (And [Always, Never]) `shouldBe` False

      it "Or combines conditions" $ do
        evalSideCondition emptySubst (Or [Always, Never]) `shouldBe` True
        evalSideCondition emptySubst (Or [Never, Never]) `shouldBe` False

      it "Not negates condition" $ do
        evalSideCondition emptySubst (Not Always) `shouldBe` False
        evalSideCondition emptySubst (Not Never) `shouldBe` True

  describe "Expression Complexity" $ do
    describe "exprComplexity" $ do
      it "simple variable has low complexity" $ do
        let (Right e) = parsePatternPure "x"
        exprComplexity e < 5 `shouldBe` True

      it "nested application increases complexity" $ do
        let (Right e1) = parsePatternPure "x"
            (Right e2) = parsePatternPure "f (g (h x))"
        exprComplexity e2 > exprComplexity e1 `shouldBe` True

  describe "containsVar" $ do
    it "finds variable in expression" $ do
      let (Right e) = parsePatternPure "f x y"
      containsVar "x" e `shouldBe` True

    it "does not find absent variable" $ do
      let (Right e) = parsePatternPure "f x y"
      containsVar "z" e `shouldBe` False

  describe "parseSideCondition" $ do
    it "parses isAtomic" $ do
      parseSideCondition "isAtomic x" `shouldBe` Just (IsAtomic "x")

    it "parses isLiteral" $ do
      parseSideCondition "isLiteral x" `shouldBe` Just (IsLiteral "x")

    it "parses notEqual" $ do
      parseSideCondition "notEqual x y" `shouldBe` Just (NotEqual "x" "y")

    it "returns Nothing for invalid" $ do
      parseSideCondition "invalid condition" `shouldBe` Nothing

  describe "Pretty Printing" $ do
    describe "prettyExpr" $ do
      it "prints simple expression" $ do
        let (Right e) = parsePatternPure "foo"
        T.null (prettyExpr e) `shouldBe` False

      it "prints complex expression" $ do
        let (Right e) = parsePatternPure "map f (filter p xs)"
        T.null (prettyExpr e) `shouldBe` False

  --------------------------------------------------------------------------------
  -- TypeEnv Tests
  --------------------------------------------------------------------------------
  describe "TypeEnv" $ do
    describe "emptyTypeEnv" $ do
      it "has no type mappings" $ do
        lookupVarType "x" emptyTypeEnv `shouldBe` Nothing
        lookupPosType 1 1 emptyTypeEnv `shouldBe` Nothing

      it "has empty module name" $ do
        teModuleName emptyTypeEnv `shouldBe` ""

    describe "mkTypeEnv" $ do
      it "creates env from type bindings" $ do
        let env = mkTypeEnv [("x", "Int"), ("y", "String")]
        lookupVarType "x" env `shouldBe` Just "Int"
        lookupVarType "y" env `shouldBe` Just "String"

      it "returns Nothing for unbound vars" $ do
        let env = mkTypeEnv [("x", "Int")]
        lookupVarType "z" env `shouldBe` Nothing

    describe "addVarType" $ do
      it "adds new type binding" $ do
        let env = addVarType "x" "Int" emptyTypeEnv
        lookupVarType "x" env `shouldBe` Just "Int"

      it "overwrites existing binding" $ do
        let env1 = addVarType "x" "Int" emptyTypeEnv
            env2 = addVarType "x" "String" env1
        lookupVarType "x" env2 `shouldBe` Just "String"

    describe "typeMatchesPattern" $ do
      it "matches exact types" $ do
        typeMatchesPattern "Int" "Int" `shouldBe` True
        typeMatchesPattern "String" "String" `shouldBe` True

      it "fails on different types" $ do
        typeMatchesPattern "Int" "String" `shouldBe` False

      it "single letter pattern matches any type" $ do
        typeMatchesPattern "Int" "a" `shouldBe` True
        typeMatchesPattern "String" "a" `shouldBe` True
        typeMatchesPattern "Maybe Int" "a" `shouldBe` True

      it "matches list patterns" $ do
        typeMatchesPattern "[Int]" "[a]" `shouldBe` True
        typeMatchesPattern "[String]" "[a]" `shouldBe` True

      it "matches Maybe patterns" $ do
        typeMatchesPattern "Maybe Int" "Maybe a" `shouldBe` True

      it "matches function patterns" $ do
        typeMatchesPattern "Int -> String" "a -> b" `shouldBe` True

    describe "typeHasClass" $ do
      it "numeric types have Num" $ do
        typeHasClass "Int" "Num" `shouldBe` True
        typeHasClass "Double" "Num" `shouldBe` True
        typeHasClass "Integer" "Num" `shouldBe` True

      it "most types have Eq" $ do
        typeHasClass "Int" "Eq" `shouldBe` True
        typeHasClass "String" "Eq" `shouldBe` True

      it "functor types have Functor" $ do
        typeHasClass "[]" "Functor" `shouldBe` True
        typeHasClass "Maybe" "Functor" `shouldBe` True
        typeHasClass "IO" "Functor" `shouldBe` True

      it "monad types have Monad" $ do
        typeHasClass "[]" "Monad" `shouldBe` True
        typeHasClass "Maybe" "Monad" `shouldBe` True
        typeHasClass "IO" "Monad" `shouldBe` True

      it "monoid types have Monoid" $ do
        typeHasClass "[]" "Monoid" `shouldBe` True  -- List type is a Monoid
        typeHasClass "[Int]" "Monoid" `shouldBe` True
        typeHasClass "Text" "Monoid" `shouldBe` True

    describe "isNumericType" $ do
      it "recognizes numeric types" $ do
        isNumericType "Int" `shouldBe` True
        isNumericType "Integer" `shouldBe` True
        isNumericType "Float" `shouldBe` True
        isNumericType "Double" `shouldBe` True
        isNumericType "Word" `shouldBe` True

      it "recognizes sized numeric types" $ do
        isNumericType "Int64" `shouldBe` True
        isNumericType "Word32" `shouldBe` True

      it "rejects non-numeric types" $ do
        isNumericType "String" `shouldBe` False
        isNumericType "Bool" `shouldBe` False

    describe "isStringType" $ do
      it "recognizes string types" $ do
        isStringType "String" `shouldBe` True
        isStringType "Text" `shouldBe` True
        isStringType "[Char]" `shouldBe` True
        isStringType "ByteString" `shouldBe` True

      it "rejects non-string types" $ do
        isStringType "Int" `shouldBe` False
        isStringType "[Int]" `shouldBe` False

    describe "isListType" $ do
      it "recognizes list types" $ do
        isListType "[Int]" `shouldBe` True
        isListType "[String]" `shouldBe` True
        isListType "[]" `shouldBe` True

      it "rejects non-list types" $ do
        isListType "Int" `shouldBe` False
        isListType "Maybe Int" `shouldBe` False

    describe "isMaybeType" $ do
      it "recognizes Maybe types" $ do
        isMaybeType "Maybe Int" `shouldBe` True
        isMaybeType "Maybe String" `shouldBe` True

      it "rejects non-Maybe types" $ do
        isMaybeType "Int" `shouldBe` False
        isMaybeType "Either Int String" `shouldBe` False

    describe "isInMonad" $ do
      it "detects monad prefix" $ do
        isInMonad "IO Int" "IO" `shouldBe` True
        isInMonad "Maybe String" "Maybe" `shouldBe` True
        isInMonad "State s a" "State" `shouldBe` True

      it "fails on different monad" $ do
        isInMonad "IO Int" "Maybe" `shouldBe` False

  --------------------------------------------------------------------------------
  -- Extended Substitution Tests
  --------------------------------------------------------------------------------
  describe "Extended Substitution Operations" $ do
    describe "emptySubst" $ do
      it "has no bindings" $ do
        isNothing (lookupSubst "x" emptySubst) `shouldBe` True
        substKeys emptySubst `shouldBe` []
        null (substValues emptySubst) `shouldBe` True

    describe "singletonSubst" $ do
      it "creates single binding" $ do
        let e = parseExpr "foo"
            s = singletonSubst "x" e
        hasBinding "x" s `shouldBe` True

      it "has exactly one key" $ do
        let e = parseExpr "foo"
            s = singletonSubst "x" e
        length (substKeys s) `shouldBe` 1

    describe "substKeys" $ do
      it "returns all bound variables" $ do
        let e1 = parseExpr "foo"
            e2 = parseExpr "bar"
            s1 = singletonSubst "x" e1
            s2 = singletonSubst "y" e2
        case mergeSubst s1 s2 of
          Just merged -> Set.fromList (substKeys merged) `shouldBe` Set.fromList ["x", "y"]
          Nothing -> fail "Merge should succeed"

    describe "substValues" $ do
      it "returns all bound expressions" $ do
        let e = parseExpr "foo"
            s = singletonSubst "x" e
        length (substValues s) `shouldBe` 1

    describe "validSubst" $ do
      it "empty subst is valid" $ do
        validSubst emptySubst `shouldBe` True

      it "any subst is valid (Map ensures uniqueness)" $ do
        let e = parseExpr "foo"
            s = singletonSubst "x" e
        validSubst s `shouldBe` True

    describe "Semigroup instance" $ do
      it "combines substitutions with (<>)" $ do
        let e1 = parseExpr "foo"
            e2 = parseExpr "bar"
            s1 = singletonSubst "x" e1
            s2 = singletonSubst "y" e2
            combined = s1 <> s2
        hasBinding "x" combined `shouldBe` True
        hasBinding "y" combined `shouldBe` True

    describe "Monoid instance" $ do
      it "mempty is emptySubst" $ do
        substKeys mempty `shouldBe` []

  --------------------------------------------------------------------------------
  -- Expression Equality Tests
  --------------------------------------------------------------------------------
  describe "Expression Equality" $ do
    describe "exprEq" $ do
      it "same variable is equal" $ do
        let e1 = parseExpr "foo"
            e2 = parseExpr "foo"
        exprEq e1 e2 `shouldBe` True

      it "different variables are not equal" $ do
        let e1 = parseExpr "foo"
            e2 = parseExpr "bar"
        exprEq e1 e2 `shouldBe` False

      it "same application is equal" $ do
        let e1 = parseExpr "f x"
            e2 = parseExpr "f x"
        exprEq e1 e2 `shouldBe` True

      it "different applications are not equal" $ do
        let e1 = parseExpr "f x"
            e2 = parseExpr "g x"
        exprEq e1 e2 `shouldBe` False

      it "same literal is equal" $ do
        let e1 = parseExpr "42"
            e2 = parseExpr "42"
        exprEq e1 e2 `shouldBe` True

      it "different literals are not equal" $ do
        let e1 = parseExpr "42"
            e2 = parseExpr "43"
        exprEq e1 e2 `shouldBe` False

      it "same string literal is equal" $ do
        let e1 = parseExpr "\"hello\""
            e2 = parseExpr "\"hello\""
        exprEq e1 e2 `shouldBe` True

      it "same list is equal" $ do
        let e1 = parseExpr "[1, 2, 3]"
            e2 = parseExpr "[1, 2, 3]"
        exprEq e1 e2 `shouldBe` True

      it "different lists are not equal" $ do
        let e1 = parseExpr "[1, 2]"
            e2 = parseExpr "[1, 2, 3]"
        exprEq e1 e2 `shouldBe` False

      it "same tuple is equal" $ do
        let e1 = parseExpr "(1, \"a\")"
            e2 = parseExpr "(1, \"a\")"
        exprEq e1 e2 `shouldBe` True

      it "same if-then-else is equal" $ do
        let e1 = parseExpr "if True then 1 else 0"
            e2 = parseExpr "if True then 1 else 0"
        exprEq e1 e2 `shouldBe` True

      it "same lambda is equal" $ do
        let e1 = parseExpr "\\x -> x"
            e2 = parseExpr "\\x -> x"
        exprEq e1 e2 `shouldBe` True

      it "same section is equal" $ do
        let e1 = parseExpr "(+ 1)"
            e2 = parseExpr "(+ 1)"
        exprEq e1 e2 `shouldBe` True

      it "same infix is equal" $ do
        let e1 = parseExpr "1 + 2"
            e2 = parseExpr "1 + 2"
        exprEq e1 e2 `shouldBe` True

      it "parentheses are transparent" $ do
        let e1 = parseExpr "(x)"
            e2 = parseExpr "x"
        -- HsPar wraps the expression, so they may not be structurally equal
        -- but unification should still work
        isJust (unifyExpr e1 e2) `shouldBe` True

  --------------------------------------------------------------------------------
  -- AST Transformation Tests
  --------------------------------------------------------------------------------
  describe "AST Transformation" $ do
    describe "applySubst / applySubstDeep" $ do
      it "substitutes variable" $ do
        let pat = parseExpr "f x"
            replacement = parseExpr "bar"
            subst = singletonSubst "x" replacement
            result = applySubst subst pat
        -- Result should have bar instead of x
        prettyExpr result `shouldSatisfy` T.isInfixOf "bar"

      it "substitutes nested variables" $ do
        let pat = parseExpr "f (g x)"
            replacement = parseExpr "42"
            subst = singletonSubst "x" replacement
            result = applySubstDeep subst pat
        prettyExpr result `shouldSatisfy` T.isInfixOf "42"

      it "leaves non-matched vars unchanged" $ do
        let pat = parseExpr "foo"
            subst = singletonSubst "x" (parseExpr "bar")
            result = applySubst subst pat
        prettyExpr result `shouldSatisfy` T.isInfixOf "foo"

    describe "normalizeExpr" $ do
      it "removes unnecessary parentheses" $ do
        let e = parseExpr "((x))"
            normalized = normalizeExpr e
        -- After normalization, should still represent x
        isJust (unifyExpr normalized (parseExpr "x")) `shouldBe` True

    describe "etaReduce" $ do
      it "reduces \\x -> f x to f" $ do
        let e = parseExpr "\\x -> f x"
            reduced = etaReduce e
        -- Should become f (check via pretty print)
        let pretty = prettyExpr reduced
        pretty `shouldSatisfy` (== "f")

      it "does not reduce when var is free in f" $ do
        let e = parseExpr "\\x -> x x"
            reduced = etaReduce e
        -- Should remain unchanged (x is free in first x)
        let pretty = prettyExpr reduced
        pretty `shouldSatisfy` T.isInfixOf "x"

  --------------------------------------------------------------------------------
  -- Pattern Unification Extended Tests
  --------------------------------------------------------------------------------
  describe "Extended Pattern Unification" $ do
    describe "unifyPat" $ do
      it "unifies variable patterns" $ do
        let (Right pat1) = parsePatternPure "x"
            (Right pat2) = parsePatternPure "y"
        -- Parse as expressions, but we test the concept
        -- For actual pattern tests we'd need pattern parsing
        isJust (unifyExpr pat1 pat2) `shouldBe` True

    describe "unifyExprs" $ do
      it "unifies empty lists" $ do
        case unifyExprs [] [] of
          Just subst -> substKeys subst `shouldBe` []
          Nothing -> fail "Should have unified empty lists"

      it "unifies single element lists" $ do
        let e1 = parseExpr "x"
            e2 = parseExpr "foo"
        case unifyExprs [e1] [e2] of
          Just subst -> hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "fails on different length lists" $ do
        let e1 = parseExpr "x"
            e2 = parseExpr "y"
            e3 = parseExpr "z"
        isNothing (unifyExprs [e1] [e2, e3]) `shouldBe` True

    describe "dollar operator handling" $ do
      it "unifies f $ x with f x" $ do
        let pat = parseExpr "f x"
            target = parseExpr "foo $ bar"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "f" subst `shouldBe` True
            hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should unify through $"

    describe "negation" $ do
      it "unifies negated expressions" $ do
        let pat = parseExpr "negate x"
            target = parseExpr "negate 5"
        case unifyExpr pat target of
          Just subst -> hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

    describe "sections" $ do
      it "unifies left section" $ do
        let pat = parseExpr "(x +)"
            target = parseExpr "(1 +)"
        case unifyExpr pat target of
          Just subst -> hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "unifies right section" $ do
        let pat = parseExpr "(+ x)"
            target = parseExpr "(+ 1)"
        case unifyExpr pat target of
          Just subst -> hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

    describe "arithmetic sequences" $ do
      it "unifies [n..]" $ do
        let pat = parseExpr "[x..]"
            target = parseExpr "[1..]"
        case unifyExpr pat target of
          Just subst -> hasBinding "x" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "unifies [n..m]" $ do
        let pat = parseExpr "[x..y]"
            target = parseExpr "[1..10]"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "x" subst `shouldBe` True
            hasBinding "y" subst `shouldBe` True
          Nothing -> fail "Should have unified"

      it "unifies [n,m..]" $ do
        let pat = parseExpr "[x, y..]"
            target = parseExpr "[1, 3..]"
        case unifyExpr pat target of
          Just subst -> do
            hasBinding "x" subst `shouldBe` True
            hasBinding "y" subst `shouldBe` True
          Nothing -> fail "Should have unified"

  --------------------------------------------------------------------------------
  -- Additional Side Condition Tests
  --------------------------------------------------------------------------------
  describe "Additional Side Conditions" $ do
    describe "isApplicationExpr" $ do
      it "recognizes function application" $ do
        let e = parseExpr "f x"
        isApplicationExpr e `shouldBe` True

      it "recognizes infix application" $ do
        let e = parseExpr "x + y"
        isApplicationExpr e `shouldBe` True

      it "rejects simple variable" $ do
        let e = parseExpr "x"
        isApplicationExpr e `shouldBe` False

      it "looks through parentheses" $ do
        let e = parseExpr "(f x)"
        isApplicationExpr e `shouldBe` True

    describe "isPure" $ do
      it "literal is pure" $ do
        let e = parseExpr "42"
        isPure e `shouldBe` True

      it "pure function is pure" $ do
        let e = parseExpr "map f xs"
        isPure e `shouldBe` True

      it "impure function is not pure" $ do
        let e = parseExpr "putStrLn x"
        isPure e `shouldBe` False

      it "print is not pure" $ do
        let e = parseExpr "print x"
        isPure e `shouldBe` False

    describe "isBindExpr" $ do
      it "recognizes >>=" $ do
        let e = parseExpr "x >>= f"
        isBindExpr e `shouldBe` True

      it "recognizes >>" $ do
        let e = parseExpr "x >> y"
        isBindExpr e `shouldBe` True

      it "rejects pure expression" $ do
        let e = parseExpr "f x"
        isBindExpr e `shouldBe` False

    describe "freeVars" $ do
      it "collects variables from expression" $ do
        let e = parseExpr "f x y"
            vars = freeVars e
        Set.member "f" vars `shouldBe` True
        Set.member "x" vars `shouldBe` True
        Set.member "y" vars `shouldBe` True

      it "returns empty for literal" $ do
        let e = parseExpr "42"
            vars = freeVars e
        Set.null vars `shouldBe` True

    describe "collectVars" $ do
      it "collects all variables" $ do
        let e = parseExpr "f (g x) y"
            vars = collectVars e
        Set.size vars `shouldBe` 4

    describe "evalSideConditionWithTypes" $ do
      it "HasType passes when type matches" $ do
        let env = mkTypeEnv [("x", "Int")]
            subst = emptySubst
        evalSideConditionWithTypes subst env (HasType "x" "Int") `shouldBe` True

      it "HasType passes when no type info (permissive)" $ do
        let env = emptyTypeEnv
            subst = emptySubst
        evalSideConditionWithTypes subst env (HasType "x" "Int") `shouldBe` True

      it "TypeMatches uses pattern matching" $ do
        let env = mkTypeEnv [("x", "Maybe Int")]
            subst = emptySubst
        evalSideConditionWithTypes subst env (TypeMatches "x" "Maybe a") `shouldBe` True

      it "HasTypeClass checks class membership" $ do
        let env = mkTypeEnv [("x", "Int")]
            subst = emptySubst
        evalSideConditionWithTypes subst env (HasTypeClass "x" "Num") `shouldBe` True

      it "IsNumeric checks numeric type" $ do
        let env = mkTypeEnv [("x", "Int")]
            subst = emptySubst
        evalSideConditionWithTypes subst env (IsNumeric "x") `shouldBe` True

      it "IsString checks string type" $ do
        let env = mkTypeEnv [("x", "Text")]
            subst = emptySubst
        evalSideConditionWithTypes subst env (IsString "x") `shouldBe` True

      it "IsList checks list type" $ do
        let env = mkTypeEnv [("x", "[Int]")]
            subst = emptySubst
        evalSideConditionWithTypes subst env (IsList "x") `shouldBe` True

      it "IsMaybe checks Maybe type" $ do
        let env = mkTypeEnv [("x", "Maybe Int")]
            subst = emptySubst
        evalSideConditionWithTypes subst env (IsMaybe "x") `shouldBe` True

      it "IsMonad checks monad context" $ do
        let env = mkTypeEnv [("x", "IO String")]
            subst = emptySubst
        evalSideConditionWithTypes subst env (IsMonad "x" "IO") `shouldBe` True

      it "ComplexityLT checks expression complexity" $ do
        let e = parseExpr "x"
            subst = singletonSubst "x" e
        evalSideCondition subst (ComplexityLT "x" 10) `shouldBe` True

      it "ComplexityGT checks expression complexity" $ do
        let e = parseExpr "f (g (h (i x)))"
            subst = singletonSubst "x" e
        evalSideCondition subst (ComplexityGT "x" 2) `shouldBe` True

      it "NotIn checks variable not in list" $ do
        let e = parseExpr "foo"
            subst = singletonSubst "x" e
        evalSideCondition subst (NotIn "x" ["bar", "baz"]) `shouldBe` True

      it "FreeIn checks variable is free" $ do
        let e1 = parseExpr "x"
            e2 = parseExpr "f x y"
            subst = singletonSubst "v" e1 <> singletonSubst "e" e2
        evalSideCondition subst (FreeIn "v" "e") `shouldBe` True

      it "NotFreeIn checks variable is not free" $ do
        let e1 = parseExpr "z"
            e2 = parseExpr "f x y"
            subst = singletonSubst "v" e1 <> singletonSubst "e" e2
        evalSideCondition subst (NotFreeIn "v" "e") `shouldBe` True

      it "IsLambda checks for lambda" $ do
        let e = parseExpr "\\x -> x"
            subst = singletonSubst "f" e
        evalSideCondition subst (IsLambda "f") `shouldBe` True

      it "IsApplication checks for application" $ do
        let e = parseExpr "f x"
            subst = singletonSubst "e" e
        evalSideCondition subst (IsApplication "e") `shouldBe` True

      it "IsPure checks for purity" $ do
        let e = parseExpr "map f xs"
            subst = singletonSubst "e" e
        evalSideCondition subst (IsPure "e") `shouldBe` True

      it "NotBind rejects bind expressions" $ do
        let e = parseExpr "x >>= f"
            subst = singletonSubst "e" e
        evalSideCondition subst (NotBind "e") `shouldBe` False

      it "NotBind passes for non-bind" $ do
        let e = parseExpr "f x"
            subst = singletonSubst "e" e
        evalSideCondition subst (NotBind "e") `shouldBe` True

  --------------------------------------------------------------------------------
  -- parseSideCondition Extended Tests
  --------------------------------------------------------------------------------
  describe "Extended parseSideCondition" $ do
    it "parses isVariable" $ do
      parseSideCondition "isVariable x" `shouldBe` Just (IsVariable "x")

    it "parses legacy isVar" $ do
      parseSideCondition "isVar x" `shouldBe` Just (IsVariable "x")

    it "parses isLambda" $ do
      parseSideCondition "isLambda f" `shouldBe` Just (IsLambda "f")

    it "parses legacy isFun" $ do
      parseSideCondition "isFun f" `shouldBe` Just (IsLambda "f")

    it "parses isApplication" $ do
      parseSideCondition "isApplication e" `shouldBe` Just (IsApplication "e")

    it "parses isConstructor" $ do
      parseSideCondition "isConstructor e" `shouldBe` Just (IsConstructor "e")

    it "parses isPure" $ do
      parseSideCondition "isPure e" `shouldBe` Just (IsPure "e")

    it "parses freeIn" $ do
      parseSideCondition "freeIn x e" `shouldBe` Just (FreeIn "x" "e")

    it "parses notFreeIn" $ do
      parseSideCondition "notFreeIn x e" `shouldBe` Just (NotFreeIn "x" "e")

    it "parses hasType" $ do
      parseSideCondition "hasType x Int" `shouldBe` Just (HasType "x" "Int")

    it "parses typeMatches" $ do
      parseSideCondition "typeMatches x Maybe" `shouldBe` Just (TypeMatches "x" "Maybe")

    it "parses hasTypeClass" $ do
      parseSideCondition "hasTypeClass x Num" `shouldBe` Just (HasTypeClass "x" "Num")

    it "parses typeContains" $ do
      parseSideCondition "typeContains x Int" `shouldBe` Just (TypeContains "x" "Int")

    it "parses isNumeric" $ do
      parseSideCondition "isNumeric x" `shouldBe` Just (IsNumeric "x")

    it "parses isString" $ do
      parseSideCondition "isString x" `shouldBe` Just (IsString "x")

    it "parses isList" $ do
      parseSideCondition "isList x" `shouldBe` Just (IsList "x")

    it "parses isMaybe" $ do
      parseSideCondition "isMaybe x" `shouldBe` Just (IsMaybe "x")

    it "parses isMonad" $ do
      parseSideCondition "isMonad x IO" `shouldBe` Just (IsMonad "x" "IO")

    it "parses hasImport" $ do
      parseSideCondition "hasImport Data.Text" `shouldBe` Just (HasImport "Data.Text")

    it "parses hasPragma" $ do
      parseSideCondition "hasPragma INLINE" `shouldBe` Just (HasPragma "INLINE")

    it "parses inModule" $ do
      parseSideCondition "inModule Test.*" `shouldBe` Just (InModule "Test.*")

    it "parses inTestFile" $ do
      parseSideCondition "inTestFile" `shouldBe` Just InTestFile

    it "parses notInTestFile" $ do
      parseSideCondition "notInTestFile" `shouldBe` Just NotInTestFile

    it "parses always" $ do
      parseSideCondition "always" `shouldBe` Just Always

    it "parses never" $ do
      parseSideCondition "never" `shouldBe` Just Never

    it "parses notInComment" $ do
      parseSideCondition "notInComment" `shouldBe` Just NotInComment

    it "parses notInString" $ do
      parseSideCondition "notInString" `shouldBe` Just NotInString

    it "parses notInImport" $ do
      parseSideCondition "notInImport" `shouldBe` Just NotInImport

    it "parses inFunctionBody" $ do
      parseSideCondition "inFunctionBody" `shouldBe` Just InFunctionBody

    it "parses legacy isAtom" $ do
      parseSideCondition "isAtom x" `shouldBe` Just (IsAtomic "x")

    it "parses legacy notEq" $ do
      parseSideCondition "notEq x y" `shouldBe` Just (NotEqual "x" "y")

  --------------------------------------------------------------------------------
  -- ASTPattern Show Instance Tests
  --------------------------------------------------------------------------------
  describe "ASTPattern" $ do
    describe "Show instance" $ do
      it "shows ExprPattern" $ do
        let e = parseExpr "f x"
            pat = ExprPattern e
        show pat `shouldSatisfy` (not . null)

    describe "MatchResult" $ do
      it "shows MatchResult" $ do
        let e = parseExpr "f x"
            mr = MatchResult emptySubst (getLocA e) e Nothing
        show mr `shouldSatisfy` (not . null)

  --------------------------------------------------------------------------------
  -- findMatches Tests
  --------------------------------------------------------------------------------
  describe "findMatches" $ do
    it "finds direct match" $ do
      let pat = parseExpr "f x"
          target = parseExpr "foo bar"
          matches = findMatches pat [] Nothing target
      length matches `shouldBe` 1

    it "finds nested match" $ do
      let pat = parseExpr "f x"
          target = parseExpr "g (foo bar)"
          matches = findMatches pat [] Nothing target
      length matches `shouldSatisfy` (>= 1)

    it "respects side conditions" $ do
      let pat = parseExpr "x"
          target = parseExpr "foo"
          -- IsAtomic should pass for 'foo'
          matches = findMatches pat [IsAtomic "x"] Nothing target
      length matches `shouldBe` 1

    it "filters by failing side conditions at top level" $ do
      let pat = parseExpr "x"
          target = parseExpr "f y"  -- application, not atomic at top level
          -- findMatches searches recursively, so it finds 2 atomic subexpressions: f and y
          -- The top-level match fails IsAtomic, but the subexpressions pass
          matches = findMatches pat [IsAtomic "x"] Nothing target
      -- We get 2 matches: one for 'f' and one for 'y' (both atomic)
      length matches `shouldBe` 2

  describe "findAllMatches" $ do
    it "works with empty rules" $ do
      let e = parseExpr "f x"
          matches = findAllMatches [] e
      length matches `shouldBe` 0

  --------------------------------------------------------------------------------
  -- matchPattern Tests
  --------------------------------------------------------------------------------
  describe "matchPattern" $ do
    it "returns substitution on match" $ do
      let pat = parseExpr "f x"
          target = parseExpr "foo bar"
      case matchPattern pat target of
        Just subst -> do
          hasBinding "f" subst `shouldBe` True
          hasBinding "x" subst `shouldBe` True
        Nothing -> fail "Should have matched"

    it "returns Nothing on mismatch" $ do
      let pat = parseExpr "map f x"
          target = parseExpr "filter p xs"
      isNothing (matchPattern pat target) `shouldBe` True
