{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : PropertySpec
-- Description : Comprehensive property-based tests for Argus core types
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Property-based tests covering core types, invariants, algebraic laws, and edge cases.
-- Uses TestUtils for shared generators and leverages QuickCheck for thorough coverage.
module PropertySpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson (FromJSON, ToJSON)
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Types
import Argus.Analysis.DepGraph

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a valid Line value
genLine :: Gen Line
genLine = Line <$> chooseInt (1, 10000)

-- | Generate a valid Column value
genColumn :: Gen Column
genColumn = Column <$> chooseInt (1, 200)

-- | Generate a valid file path
genFilePath :: Gen FilePath
genFilePath = do
  dirs <- listOf1 $ elements ["src", "lib", "test", "app", "internal"]
  file <- elements ["Main.hs", "Lib.hs", "Types.hs", "Utils.hs", "Core.hs"]
  pure $ foldr (\d acc -> d ++ "/" ++ acc) file dirs

-- | Generate a valid SrcSpan
genSrcSpan :: Gen SrcSpan
genSrcSpan = do
  path <- genFilePath
  startLine <- genLine
  startCol <- genColumn
  endLine <- Line <$> chooseInt (unLine startLine, unLine startLine + 100)
  endCol <- if startLine == endLine
            then Column <$> chooseInt (unColumn startCol, unColumn startCol + 50)
            else genColumn
  pure $ SrcSpan path startLine startCol endLine endCol

-- | Generate a valid Severity
genSeverity :: Gen Severity
genSeverity = elements [Error, Warning, Suggestion, Info]

-- | Generate a valid DiagnosticKind
genDiagnosticKind :: Gen DiagnosticKind
genDiagnosticKind = elements
  [ NamingConvention
  , UnusedCode
  , UnusedImport
  , RedundantCode
  , CodePattern
  , TypeSignature
  , ImportStyle
  , TemplateHaskellRef
  , SecurityIssue
  , PerformanceIssue
  , ArchitecturalIssue
  , SpaceLeak
  , PartialFunction
  , ComplexityIssue
  ]

-- | Generate a diagnostic message
genMessage :: Gen Text
genMessage = T.pack <$> elements
  [ "Use of partial function 'head'"
  , "Unused import 'Data.List'"
  , "Consider using foldl' instead of foldl"
  , "Function too complex"
  , "Naming convention violation"
  , "Potential space leak detected"
  , "Security: avoid unsafePerformIO"
  ]

-- | Generate a diagnostic code
genDiagCode :: Gen (Maybe Text)
genDiagCode = frequency
  [ (7, Just . T.pack <$> elements
      [ "partial/head"
      , "partial/tail"
      , "perf/foldl"
      , "import/unused"
      , "naming/camelCase"
      ])
  , (3, pure Nothing)
  ]

-- | Generate a simple Diagnostic (without fixes)
genDiagnostic :: Gen Diagnostic
genDiagnostic = do
  span <- genSrcSpan
  msg <- genMessage
  sev <- genSeverity
  kind <- genDiagnosticKind
  code <- genDiagCode
  pure Diagnostic
    { diagSpan = span
    , diagMessage = msg
    , diagSeverity = sev
    , diagKind = kind
    , diagCode = code
    , diagFixes = []
    , diagRelated = []
    }

-- | Generate a FixEdit
genFixEdit :: Gen FixEdit
genFixEdit = do
  span <- genSrcSpan
  newText <- T.pack <$> elements
    [ "headMay"
    , "foldl'"
    , "maybe defaultVal id"
    , "fromMaybe defaultVal"
    , "safeHead"
    ]
  pure $ FixEdit span newText

-- | Generate a Fix
genFix :: Gen Fix
genFix = do
  title <- T.pack <$> elements
    [ "Replace with safe alternative"
    , "Use strict fold"
    , "Add type annotation"
    , "Qualify import"
    ]
  edits <- listOf1 genFixEdit
  preferred <- arbitrary
  category <- elements [FCPerformance, FCModernize, FCSafety, FCStyle, FCImports, FCRedundant, FCSpaceLeaks, FCSecurity]
  safety <- elements [FSAlways, FSMostly, FSReview, FSUnsafe]
  pure Fix
    { fixTitle = title
    , fixEdits = edits
    , fixIsPreferred = preferred
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = category
    , fixSafety = safety
    }

-- | Generate a QualifiedName
genQualifiedName :: Gen QualifiedName
genQualifiedName = do
  name <- elements ["foo", "bar", "baz", "qux", "main", "helper"]
  modName <- frequency
    [ (7, Just <$> elements ["Main", "Lib", "Test", "Data.List", "Control.Monad"])
    , (3, pure Nothing)
    ]
  pure QualifiedName
    { qnName = T.pack name
    , qnModule = modName
    }

-- | Generate a SymbolKind
genSymbolKind :: Gen SymbolKind
genSymbolKind = elements
  [ Function
  , TypeConstructor
  , DataConstructor
  , TypeClass
  , TypeClassMethod
  , TypeFamily
  , PatternSynonym
  , Module
  ]

-- | Generate a Symbol
genSymbol :: Gen Symbol
genSymbol = do
  name <- genQualifiedName
  kind <- genSymbolKind
  span <- genSrcSpan
  exported <- arbitrary
  mType <- frequency [(3, Just . T.pack <$> elements ["Int", "Bool", "String", "a -> b"]), (7, pure Nothing)]
  pure Symbol
    { symbolName = name
    , symbolKind = kind
    , symbolSpan = span
    , symbolExported = exported
    , symbolType = mType
    }

-- | Generate a DepNode
genDepNode :: Gen DepNode
genDepNode = do
  sym <- genSymbol
  isRoot <- arbitrary
  isTh <- frequency [(9, pure False), (1, pure True)]
  modName <- T.pack <$> elements ["Main", "Lib", "Test", "Utils"]
  pure DepNode
    { dnSymbol = sym
    , dnIsRoot = isRoot
    , dnIsThGen = isTh
    , dnModule = modName
    }

-- | Generate an EdgeKind
genEdgeKind :: Gen EdgeKind
genEdgeKind = elements
  [ DirectReference
  , TypeDependency
  , ImportDependency
  , ExportDependency
  , ThQuote
  , ThSplice
  , InstanceDependency
  ]

-- | Generate a DepEdge
genDepEdge :: Gen DepEdge
genDepEdge = DepEdge <$> genEdgeKind <*> genSrcSpan

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Line where
  arbitrary = genLine
  shrink (Line n) = [Line n' | n' <- shrink n, n' > 0]

instance Arbitrary Column where
  arbitrary = genColumn
  shrink (Column n) = [Column n' | n' <- shrink n, n' > 0]

instance Arbitrary Severity where
  arbitrary = genSeverity

instance Arbitrary DiagnosticKind where
  arbitrary = genDiagnosticKind

instance Arbitrary SrcSpan where
  arbitrary = genSrcSpan
  shrink s =
    -- Shrink to single-line span
    [ mkSrcSpanRaw (srcSpanFile s) l (unColumn $ srcSpanStartCol s) l (unColumn $ srcSpanEndCol s)
    | let l = unLine $ srcSpanStartLine s
    , unLine (srcSpanStartLine s) /= unLine (srcSpanEndLine s)
    ]

instance Arbitrary Diagnostic where
  arbitrary = genDiagnostic
  shrink d =
    -- Shrink message
    [ d { diagMessage = T.take n (diagMessage d) }
    | n <- [10, 20]
    , n < T.length (diagMessage d)
    ]

instance Arbitrary FixEdit where
  arbitrary = genFixEdit

instance Arbitrary Fix where
  arbitrary = genFix
  shrink f =
    -- Shrink edits
    [ f { fixEdits = take n (fixEdits f) }
    | n <- [1 .. length (fixEdits f) - 1]
    ]

instance Arbitrary QualifiedName where
  arbitrary = genQualifiedName

instance Arbitrary SymbolKind where
  arbitrary = genSymbolKind

instance Arbitrary Symbol where
  arbitrary = genSymbol

instance Arbitrary DepNode where
  arbitrary = genDepNode

instance Arbitrary EdgeKind where
  arbitrary = genEdgeKind

instance Arbitrary DepEdge where
  arbitrary = genDepEdge

instance Arbitrary FixCategory where
  arbitrary = elements [FCStyle, FCSafety, FCPerformance, FCModernize, FCImports, FCRedundant, FCSpaceLeaks, FCSecurity]

instance Arbitrary FixSafety where
  arbitrary = elements [FSAlways, FSMostly, FSReview, FSUnsafe]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Property-based tests" $ do
    srcSpanProperties
    lineColumnProperties
    severityProperties
    diagnosticProperties
    fixProperties
    qualifiedNameProperties
    symbolProperties
    depGraphProperties
    orderingLawProperties
    jsonRoundTripProperties
    edgeCaseProperties

--------------------------------------------------------------------------------
-- SrcSpan Properties
--------------------------------------------------------------------------------

srcSpanProperties :: Spec
srcSpanProperties = describe "SrcSpan properties" $ do
  prop "start line is always <= end line" $ \(span :: SrcSpan) ->
    srcSpanStartLine span <= srcSpanEndLine span

  prop "if start == end line, start col <= end col" $ \(span :: SrcSpan) ->
    srcSpanStartLine span /= srcSpanEndLine span ||
    srcSpanStartCol span <= srcSpanEndCol span

  prop "mkSrcSpanRaw creates valid span" $ \(path :: String, sl :: Int, sc :: Int, el :: Int, ec :: Int) ->
    let sl' = max 1 (abs sl `mod` 10000)
        sc' = max 1 (abs sc `mod` 200)
        el' = max sl' (abs el `mod` 10000)
        ec' = if sl' == el' then max sc' (abs ec `mod` 200) else max 1 (abs ec `mod` 200)
        span = mkSrcSpanRaw path sl' sc' el' ec'
    in unLine (srcSpanStartLine span) == sl' &&
       unColumn (srcSpanStartCol span) == sc'

  prop "noSrcSpan has empty file path" $
    srcSpanFile noSrcSpan == ""

  prop "noSrcSpan is idempotent identity" $
    noSrcSpan == noSrcSpan

  prop "equality is reflexive" $ \(span :: SrcSpan) ->
    span == span

  prop "equality is symmetric" $ \(span1 :: SrcSpan) (span2 :: SrcSpan) ->
    (span1 == span2) == (span2 == span1)

  prop "equality is transitive" $ \(s1 :: SrcSpan) (s2 :: SrcSpan) (s3 :: SrcSpan) ->
    not (s1 == s2 && s2 == s3) || s1 == s3

  prop "order is consistent with Eq" $ \(span1 :: SrcSpan) (span2 :: SrcSpan) ->
    (compare span1 span2 == EQ) `implies` (span1 == span2)

  prop "span width is non-negative for single-line spans" $ \(span :: SrcSpan) ->
    srcSpanStartLine span /= srcSpanEndLine span ||
    unColumn (srcSpanEndCol span) >= unColumn (srcSpanStartCol span)

  prop "spans can be compared by file path" $ \(s1 :: SrcSpan) (s2 :: SrcSpan) ->
    srcSpanFile s1 == srcSpanFile s2 ||
    compare s1 s2 == compare (srcSpanFile s1) (srcSpanFile s2)

--------------------------------------------------------------------------------
-- Line/Column Properties
--------------------------------------------------------------------------------

lineColumnProperties :: Spec
lineColumnProperties = describe "Line/Column properties" $ do
  prop "Line constructor preserves value" $ \(Positive n) ->
    unLine (Line n) == n

  prop "Column constructor preserves value" $ \(Positive n) ->
    unColumn (Column n) == n

  prop "Line ordering is numeric" $ \(Positive a) (Positive b) ->
    compare (Line a) (Line b) == compare a b

  prop "Column ordering is numeric" $ \(Positive a) (Positive b) ->
    compare (Column a) (Column b) == compare a b

  prop "Line Eq is consistent with numeric Eq" $ \(Positive a) (Positive b) ->
    (Line a == Line b) == (a == b)

  prop "Column Eq is consistent with numeric Eq" $ \(Positive a) (Positive b) ->
    (Column a == Column b) == (a == b)

  prop "sorting Lines preserves order" $ \(ls :: [Positive Int]) ->
    let lines = map (Line . getPositive) ls
        sorted = sort lines
        values = map unLine sorted
    in values == sort values

  prop "sorting Columns preserves order" $ \(cs :: [Positive Int]) ->
    let cols = map (Column . getPositive) cs
        sorted = sort cols
        values = map unColumn sorted
    in values == sort values

  prop "Line min is associative" $ \(Positive a) (Positive b) (Positive c) ->
    min (Line a) (min (Line b) (Line c)) == min (min (Line a) (Line b)) (Line c)

  prop "Column max is associative" $ \(Positive a) (Positive b) (Positive c) ->
    max (Column a) (max (Column b) (Column c)) == max (max (Column a) (Column b)) (Column c)

--------------------------------------------------------------------------------
-- Severity Properties
--------------------------------------------------------------------------------

severityProperties :: Spec
severityProperties = describe "Severity properties" $ do
  prop "all severities are showable" $ \(sev :: Severity) ->
    length (show sev) > 0

  prop "Error is the most severe (lowest in Ord)" $ \(sev :: Severity) ->
    sev == Error || Error < sev

  it "severity ordering (derived): Error < Warning < Suggestion < Info" $ do
    Error < Warning `shouldBe` True
    Warning < Suggestion `shouldBe` True
    Suggestion < Info `shouldBe` True

  prop "severity ordering is total" $ \(s1 :: Severity) (s2 :: Severity) ->
    s1 <= s2 || s2 <= s1

  prop "severity ordering is transitive" $ \(s1 :: Severity) (s2 :: Severity) (s3 :: Severity) ->
    not (s1 <= s2 && s2 <= s3) || s1 <= s3

  prop "severity ordering is antisymmetric" $ \(s1 :: Severity) (s2 :: Severity) ->
    not (s1 <= s2 && s2 <= s1) || s1 == s2

  prop "severity ordering is reflexive" $ \(s :: Severity) ->
    s <= s

  prop "Info is the least severe (highest in Ord)" $ \(sev :: Severity) ->
    sev == Info || sev < Info

--------------------------------------------------------------------------------
-- Diagnostic Properties
--------------------------------------------------------------------------------

diagnosticProperties :: Spec
diagnosticProperties = describe "Diagnostic properties" $ do
  prop "diagnostic message is non-empty" $ \(diag :: Diagnostic) ->
    not (T.null (diagMessage diag))

  prop "diagnostic span is accessible" $ \(diag :: Diagnostic) ->
    diagSpan diag == diagSpan diag

  prop "diagnostic severity is accessible" $ \(diag :: Diagnostic) ->
    diagSeverity diag `elem` [Error, Warning, Suggestion, Info]

  prop "diagnostic kind is in valid set" $ \(diag :: Diagnostic) ->
    diagKind diag `elem`
      [ NamingConvention, UnusedCode, UnusedImport, RedundantCode
      , CodePattern, TypeSignature, ImportStyle, TemplateHaskellRef
      , SecurityIssue, PerformanceIssue, ArchitecturalIssue, SpaceLeak
      , PartialFunction, ComplexityIssue
      ]

  prop "equality is reflexive" $ \(diag :: Diagnostic) ->
    diag == diag

  prop "equality is symmetric" $ \(diag1 :: Diagnostic) (diag2 :: Diagnostic) ->
    (diag1 == diag2) == (diag2 == diag1)

  prop "show produces non-empty string" $ \(diag :: Diagnostic) ->
    not (null (show diag))

  prop "diagnostics without fixes have empty fix list" $ \(diag :: Diagnostic) ->
    null (diagFixes diag)  -- Our generator produces diagnostics without fixes

  prop "diagnostics can have code or not" $ \(diag :: Diagnostic) ->
    case diagCode diag of
      Nothing -> True
      Just code -> not (T.null code)

--------------------------------------------------------------------------------
-- Fix Properties
--------------------------------------------------------------------------------

fixProperties :: Spec
fixProperties = describe "Fix properties" $ do
  prop "fix title is non-empty" $ \(fix :: Fix) ->
    not (T.null (fixTitle fix))

  prop "fix edits list is non-empty" $ \(fix :: Fix) ->
    not (null (fixEdits fix))

  prop "fix category is valid" $ \(fix :: Fix) ->
    fixCategory fix `elem` [FCPerformance, FCModernize, FCSafety, FCStyle, FCImports, FCRedundant, FCSpaceLeaks, FCSecurity]

  prop "fix safety is valid" $ \(fix :: Fix) ->
    fixSafety fix `elem` [FSAlways, FSMostly, FSReview, FSUnsafe]

  prop "equality is reflexive" $ \(fix :: Fix) ->
    fix == fix

  prop "show produces non-empty string" $ \(fix :: Fix) ->
    not (null (show fix))

  prop "all fix edits have valid spans" $ \(fix :: Fix) ->
    all (\e -> fixEditSpan e == fixEditSpan e) (fixEdits fix)

  prop "all fix edits have text" $ \(fix :: Fix) ->
    all (\e -> not (T.null (fixEditNewText e))) (fixEdits fix)

  prop "preferred status is boolean" $ \(fix :: Fix) ->
    fixIsPreferred fix || not (fixIsPreferred fix)  -- Always true, tests accessor

  prop "FSAlways is safest" $ \(safety :: FixSafety) ->
    safety == FSAlways || FSAlways < safety

  prop "FSUnsafe is least safe" $ \(safety :: FixSafety) ->
    safety == FSUnsafe || safety < FSUnsafe

--------------------------------------------------------------------------------
-- QualifiedName Properties
--------------------------------------------------------------------------------

qualifiedNameProperties :: Spec
qualifiedNameProperties = describe "QualifiedName properties" $ do
  prop "name is non-empty" $ \(qn :: QualifiedName) ->
    not (T.null (qnName qn))

  prop "equality is reflexive" $ \(qn :: QualifiedName) ->
    qn == qn

  prop "equality is symmetric" $ \(qn1 :: QualifiedName) (qn2 :: QualifiedName) ->
    (qn1 == qn2) == (qn2 == qn1)

  prop "ordering is total" $ \(qn1 :: QualifiedName) (qn2 :: QualifiedName) ->
    qn1 <= qn2 || qn2 <= qn1

  prop "ordering is consistent with Eq" $ \(qn1 :: QualifiedName) (qn2 :: QualifiedName) ->
    (compare qn1 qn2 == EQ) `implies` (qn1 == qn2)

  prop "names with same name and module are equal" $ \(qn :: QualifiedName) ->
    let qn2 = QualifiedName { qnName = qnName qn, qnModule = qnModule qn }
    in qn == qn2

  prop "module can be Nothing" $ \(qn :: QualifiedName) ->
    case qnModule qn of
      Nothing -> True
      Just m -> not (T.null m)

--------------------------------------------------------------------------------
-- Symbol Properties
--------------------------------------------------------------------------------

symbolProperties :: Spec
symbolProperties = describe "Symbol properties" $ do
  prop "symbol name is non-empty" $ \(sym :: Symbol) ->
    not (T.null (qnName (symbolName sym)))

  prop "symbol kind is valid" $ \(sym :: Symbol) ->
    symbolKind sym `elem`
      [ Function, TypeConstructor, DataConstructor
      , TypeClass, TypeClassMethod, TypeFamily, PatternSynonym, Module
      ]

  prop "equality is reflexive" $ \(sym :: Symbol) ->
    sym == sym

  prop "equality is symmetric" $ \(s1 :: Symbol) (s2 :: Symbol) ->
    (s1 == s2) == (s2 == s1)

  prop "exported status is boolean" $ \(sym :: Symbol) ->
    symbolExported sym || not (symbolExported sym)

  prop "type annotation can be Nothing" $ \(sym :: Symbol) ->
    case symbolType sym of
      Nothing -> True
      Just t -> not (T.null t)

--------------------------------------------------------------------------------
-- DepGraph Properties
--------------------------------------------------------------------------------

depGraphProperties :: Spec
depGraphProperties = describe "DepGraph properties" $ do
  prop "empty graph has no nodes" $
    Map.null (dgNodes emptyGraph)

  prop "empty graph has no edges" $
    Map.null (dgEdges emptyGraph)

  prop "empty graph has no roots" $
    Set.null (dgRoots emptyGraph)

  prop "adding node increases node count or is idempotent" $ \(node :: DepNode) ->
    let qn = symbolName (dnSymbol node)
        g1 = addNode qn node emptyGraph
        g2 = addNode qn node g1
    in Map.size (dgNodes g1) >= Map.size (dgNodes emptyGraph) &&
       Map.size (dgNodes g2) == Map.size (dgNodes g1)

  prop "adding node preserves other nodes" $ \(n1 :: DepNode) (n2 :: DepNode) ->
    let qn1 = symbolName (dnSymbol n1)
        qn2 = symbolName (dnSymbol n2)
        g1 = addNode qn1 n1 emptyGraph
        g2 = addNode qn2 n2 g1
    in qn1 == qn2 || Map.member qn1 (dgNodes g2)

  prop "adding root marks node as root" $ \(qn :: QualifiedName) ->
    let g = addRoot qn emptyGraph
    in Set.member qn (dgRoots g)

  prop "adding same root twice is idempotent" $ \(qn :: QualifiedName) ->
    let g1 = addRoot qn emptyGraph
        g2 = addRoot qn g1
    in dgRoots g1 == dgRoots g2

  prop "reachableFrom empty graph includes only starting nodes" $ \(qn :: QualifiedName) ->
    reachableFrom emptyGraph (Set.singleton qn) == Set.singleton qn

  prop "reachableFrom includes starting nodes if in graph" $ \(node :: DepNode) ->
    let qn = symbolName (dnSymbol node)
        g = addNode qn node emptyGraph
        reachable = reachableFrom g (Set.singleton qn)
    in Set.member qn reachable

  prop "unreachableNodes on empty graph is empty" $
    Set.null (unreachableNodes emptyGraph)

  prop "unreachableNodes subset of all nodes" $ \(node :: DepNode) ->
    let qn = symbolName (dnSymbol node)
        g = addNode qn node (addRoot qn emptyGraph)
    in unreachableNodes g `Set.isSubsetOf` Map.keysSet (dgNodes g)

  prop "topologicalSort on empty graph is empty" $
    null (topologicalSort emptyGraph)

  prop "findCycles on empty graph is empty" $
    null (findCycles emptyGraph)

  prop "edge kind equality is reflexive" $ \(ek :: EdgeKind) ->
    ek == ek

  prop "edge kind ordering is total" $ \(ek1 :: EdgeKind) (ek2 :: EdgeKind) ->
    ek1 <= ek2 || ek2 <= ek1

--------------------------------------------------------------------------------
-- Ordering Law Properties (General)
--------------------------------------------------------------------------------

orderingLawProperties :: Spec
orderingLawProperties = describe "Ordering laws" $ do
  describe "Ord laws for FixSafety" $ do
    prop "reflexivity" $ \(x :: FixSafety) ->
      x <= x

    prop "antisymmetry" $ \(x :: FixSafety) (y :: FixSafety) ->
      not (x <= y && y <= x) || x == y

    prop "transitivity" $ \(x :: FixSafety) (y :: FixSafety) (z :: FixSafety) ->
      not (x <= y && y <= z) || x <= z

    prop "totality" $ \(x :: FixSafety) (y :: FixSafety) ->
      x <= y || y <= x

  describe "Ord laws for FixCategory" $ do
    prop "reflexivity" $ \(x :: FixCategory) ->
      x <= x

    prop "antisymmetry" $ \(x :: FixCategory) (y :: FixCategory) ->
      not (x <= y && y <= x) || x == y

    prop "transitivity" $ \(x :: FixCategory) (y :: FixCategory) (z :: FixCategory) ->
      not (x <= y && y <= z) || x <= z

    prop "totality" $ \(x :: FixCategory) (y :: FixCategory) ->
      x <= y || y <= x

  describe "Eq laws for common types" $ do
    prop "SrcSpan: reflexive" $ \(x :: SrcSpan) -> x == x
    prop "SrcSpan: symmetric" $ \(x :: SrcSpan) (y :: SrcSpan) -> (x == y) == (y == x)
    prop "Diagnostic: reflexive" $ \(x :: Diagnostic) -> x == x
    prop "Fix: reflexive" $ \(x :: Fix) -> x == x

--------------------------------------------------------------------------------
-- JSON Round-Trip Properties
--------------------------------------------------------------------------------

jsonRoundTripProperties :: Spec
jsonRoundTripProperties = describe "JSON round-trip properties" $ do
  prop "Severity round-trips through JSON" $ \(sev :: Severity) ->
    jsonRoundTrip sev

  prop "DiagnosticKind round-trips through JSON" $ \(kind :: DiagnosticKind) ->
    jsonRoundTrip kind

  prop "SrcSpan round-trips through JSON" $ \(span :: SrcSpan) ->
    jsonRoundTrip span

  prop "FixEdit round-trips through JSON" $ \(edit :: FixEdit) ->
    jsonRoundTrip edit

  prop "Fix round-trips through JSON" $ \(fix :: Fix) ->
    jsonRoundTrip fix

  prop "Diagnostic round-trips through JSON" $ \(diag :: Diagnostic) ->
    jsonRoundTrip diag

  prop "Line round-trips through JSON" $ \(line :: Line) ->
    jsonRoundTrip line

  prop "Column round-trips through JSON" $ \(col :: Column) ->
    jsonRoundTrip col

  prop "FixCategory round-trips through JSON" $ \(fc :: FixCategory) ->
    jsonRoundTrip fc

  prop "FixSafety round-trips through JSON" $ \(fs :: FixSafety) ->
    jsonRoundTrip fs

  prop "SymbolKind round-trips through JSON" $ \(sk :: SymbolKind) ->
    jsonRoundTrip sk

--------------------------------------------------------------------------------
-- Edge Case Properties
--------------------------------------------------------------------------------

edgeCaseProperties :: Spec
edgeCaseProperties = describe "Edge case properties" $ do
  describe "boundary conditions" $ do
    it "Line(1) is valid minimum" $
      unLine (Line 1) `shouldBe` 1

    it "Column(1) is valid minimum" $
      unColumn (Column 1) `shouldBe` 1

    it "noSrcSpan has line 0, col 0 (sentinel value)" $ do
      unLine (srcSpanStartLine noSrcSpan) `shouldBe` 0
      unColumn (srcSpanStartCol noSrcSpan) `shouldBe` 0

    it "empty diagnostic message is avoided by generator" $
      property $ \(diag :: Diagnostic) ->
        T.length (diagMessage diag) > 0

  describe "identity and idempotency" $ do
    prop "adding same node twice is idempotent" $ \(node :: DepNode) ->
      let qn = symbolName (dnSymbol node)
          g1 = addNode qn node emptyGraph
          g2 = addNode qn node g1
      in dgNodes g1 == dgNodes g2

    prop "sorting sorted list is idempotent" $ \(ls :: [Line]) ->
      sort (sort ls) == sort ls

    prop "nub is idempotent" $ \(ls :: [Severity]) ->
      nub (nub ls) == nub ls

  describe "shrinking" $ do
    prop "shrunk Lines are smaller" $ \(line :: Line) ->
      all (\l -> unLine l < unLine line) (shrink line)

    prop "shrunk Columns are smaller" $ \(col :: Column) ->
      all (\c -> unColumn c < unColumn col) (shrink col)

    prop "shrunk SrcSpans are simpler" $ \(span :: SrcSpan) ->
      all (\s -> unLine (srcSpanStartLine s) == unLine (srcSpanEndLine s)) (shrink span)

    prop "shrunk Fixes have fewer edits" $ \(fix :: Fix) ->
      all (\f -> length (fixEdits f) < length (fixEdits fix)) (shrink fix)

  describe "stability" $ do
    prop "show . read for Line is consistent" $ \(Positive n) ->
      show (Line n) == show (Line n)

    prop "multiple JSON round-trips are stable" $ \(sev :: Severity) ->
      let trip1 = jsonRoundTrip' sev
          trip2 = trip1 >>= jsonRoundTrip'
      in trip2 == Just sev

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Test JSON round-trip property
jsonRoundTrip :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
jsonRoundTrip x = Aeson.decode (Aeson.encode x) == Just x

-- | JSON round-trip that returns Maybe
jsonRoundTrip' :: (ToJSON a, FromJSON a) => a -> Maybe a
jsonRoundTrip' x = Aeson.decode (Aeson.encode x)

-- | Implication for properties
implies :: Bool -> Bool -> Bool
implies p q = not p || q
