{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Refactor.DeclarationSort
-- Description : Sort and organize module declarations
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functionality to sort and organize Haskell module
-- declarations according to various strategies:
--
-- * Alphabetical sorting
-- * Type-first ordering (types → functions → instances)
-- * Dependency-based ordering
-- * Custom grouping
--
-- The refactoring preserves comments associated with declarations and
-- maintains proper spacing between declaration groups.
module Argus.Refactor.DeclarationSort
  ( -- * Configuration
    SortConfig (..)
  , SortStrategy (..)
  , GroupingStyle (..)
  , defaultSortConfig
  , defaultCategoryOrder

    -- * Main API
  , sortDeclarations
  , previewSort
  , SortResult (..)

    -- * Analysis
  , analyzeDeclarations
  , DeclarationAnalysis (..)
  , DeclarationInfo (..)
  , DeclCategory (..)

    -- * Sorting strategies
  , sortAlphabetical
  , sortTypeFirst
  , sortDependency
  , sortCustom

    -- * Fix generation
  , generateSortFix
  , renderDeclarations

    -- * Utilities
  , extractDeclarations
  , categorizeDecl
  , getDeclName
  , getDeclDependencies
  , groupByCategory
  ) where

import Data.ByteString qualified as BS
import Data.List (sortBy, partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import "ghc-lib-parser" GHC.Hs
import "ghc-lib-parser" GHC.Types.Name.Occurrence (occNameString)
import "ghc-lib-parser" GHC.Types.Name.Reader (RdrName(..), rdrNameOcc)
import "ghc-lib-parser" GHC.Types.SrcLoc (GenLocated(..), unLoc)
import "ghc-lib-parser" GHC.Data.Bag (bagToList)

import Argus.Types
import Argus.Analysis.Syntactic (parseModule, ParseResult(..), spanToSrcSpan)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for declaration sorting
data SortConfig = SortConfig
  { scStrategy        :: SortStrategy
  , scGroupingStyle   :: GroupingStyle
  , scPreserveGroups  :: Bool           -- ^ Keep existing groups together
  , scBlankLinesBetweenGroups :: Int    -- ^ Blank lines between declaration groups
  , scSortWithinGroups :: Bool          -- ^ Sort declarations within each group
  , scKeepSignaturesWithBindings :: Bool  -- ^ Keep type signatures with their bindings
  , scCategoryOrder   :: [DeclCategory] -- ^ Custom category order
  }
  deriving stock (Eq, Show)

-- | Default category order for type-first sorting
defaultCategoryOrder :: [DeclCategory]
defaultCategoryOrder = [DCTypeSig, DCTypeDecl, DCDataDecl, DCNewtypeDecl,
                        DCClassDecl, DCFunctionDecl, DCInstanceDecl, DCOtherDecl]

-- | Sorting strategy
data SortStrategy
  = Alphabetical      -- ^ Sort all declarations alphabetically
  | TypeFirst         -- ^ Types first, then functions, then instances
  | DependencyOrder   -- ^ Order by dependencies (used-before-user)
  | CustomOrder       -- ^ Use custom category order from config
  | NoSort            -- ^ Don't sort, just analyze
  deriving stock (Eq, Show, Enum, Bounded)

-- | How to group declarations
data GroupingStyle
  = NoGrouping        -- ^ Keep declarations as one flat list
  | GroupByCategory   -- ^ Group by declaration category
  | GroupByPrefix     -- ^ Group by name prefix (e.g., all "handle*" together)
  | GroupByDependency -- ^ Group by dependency clusters
  deriving stock (Eq, Show, Enum, Bounded)

-- | Default sorting configuration
defaultSortConfig :: SortConfig
defaultSortConfig = SortConfig
  { scStrategy = TypeFirst
  , scGroupingStyle = GroupByCategory
  , scPreserveGroups = True
  , scBlankLinesBetweenGroups = 1
  , scSortWithinGroups = True
  , scKeepSignaturesWithBindings = True
  , scCategoryOrder = defaultCategoryOrder
  }

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result of a sorting operation
data SortResult = SortResult
  { srSuccess      :: Bool           -- ^ Did the sort succeed?
  , srFix          :: Maybe Fix      -- ^ The generated fix
  , srChanges      :: Int            -- ^ Number of declarations moved
  , srGroups       :: Int            -- ^ Number of groups created
  , srWarnings     :: [Text]         -- ^ Warning messages
  , srPreview      :: Maybe Text     -- ^ Preview of sorted content
  }
  deriving stock (Eq, Show)

-- | Analysis of declarations in a module
data DeclarationAnalysis = DeclarationAnalysis
  { daDeclarations    :: [DeclarationInfo]     -- ^ All declarations found
  , daCategories      :: Map DeclCategory [DeclarationInfo]  -- ^ Grouped by category
  , daDependencies    :: Map Text (Set Text)   -- ^ Dependency graph
  , daCurrentOrder    :: [Text]                -- ^ Current order of declarations
  , daSuggestedOrder  :: [Text]                -- ^ Suggested order
  , daIsWellOrdered   :: Bool                  -- ^ Is already well-ordered?
  }
  deriving stock (Eq, Show)

-- | Information about a single declaration
data DeclarationInfo = DeclarationInfo
  { diName          :: Text          -- ^ Declaration name
  , diCategory      :: DeclCategory  -- ^ What kind of declaration
  , diSpan          :: SrcSpan       -- ^ Source location
  , diStartLine     :: Int           -- ^ Start line number
  , diEndLine       :: Int           -- ^ End line number
  , diText          :: Text          -- ^ Original source text
  , diDependencies  :: Set Text      -- ^ Names this declaration uses
  , diAssociatedSig :: Maybe SrcSpan -- ^ Associated type signature location
  , diCommentBefore :: Maybe Text    -- ^ Comment immediately before
  }
  deriving stock (Eq, Show)

-- | Categories of declarations
data DeclCategory
  = DCTypeSig       -- ^ Type signature
  | DCTypeDecl      -- ^ Type alias
  | DCDataDecl      -- ^ Data type
  | DCNewtypeDecl   -- ^ Newtype
  | DCClassDecl     -- ^ Type class
  | DCFunctionDecl  -- ^ Function binding
  | DCInstanceDecl  -- ^ Instance declaration
  | DCOtherDecl     -- ^ Other declarations
  deriving stock (Eq, Show, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Main API
--------------------------------------------------------------------------------

-- | Sort declarations in a file
sortDeclarations :: FilePath -> SortConfig -> IO SortResult
sortDeclarations file config = do
  content <- TE.decodeUtf8 <$> BS.readFile file
  analysis <- analyzeDeclarations file content config

  if daIsWellOrdered analysis
  then pure SortResult
    { srSuccess = True
    , srFix = Nothing
    , srChanges = 0
    , srGroups = Map.size (daCategories analysis)
    , srWarnings = []
    , srPreview = Nothing
    }
  else do
    let fix = generateSortFix file content config analysis
        newContent = applySort content analysis config
    pure SortResult
      { srSuccess = True
      , srFix = Just fix
      , srChanges = countChanges (daCurrentOrder analysis) (daSuggestedOrder analysis)
      , srGroups = Map.size (daCategories analysis)
      , srWarnings = []
      , srPreview = Just newContent
      }

-- | Preview sorting without applying
previewSort :: FilePath -> SortConfig -> IO SortResult
previewSort file config = do
  content <- TE.decodeUtf8 <$> BS.readFile file
  analysis <- analyzeDeclarations file content config

  let newContent = applySort content analysis config
  pure SortResult
    { srSuccess = True
    , srFix = Just (generateSortFix file content config analysis)
    , srChanges = countChanges (daCurrentOrder analysis) (daSuggestedOrder analysis)
    , srGroups = Map.size (daCategories analysis)
    , srWarnings = []
    , srPreview = Just newContent
    }

--------------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------------

-- | Analyze declarations in a module
analyzeDeclarations :: FilePath -> Text -> SortConfig -> IO DeclarationAnalysis
analyzeDeclarations file content config = do
  parseResult <- parseModule file content

  case parseResult of
    Left _err -> pure emptyAnalysis
    Right parsed -> do
      let decls = extractDeclarations file content (prModule parsed)
          grouped = groupByCategory decls
          deps = buildDependencyMap decls
          currentOrder = map diName decls
          suggestedOrder = computeSuggestedOrder config decls deps
          isWellOrdered = currentOrder == suggestedOrder

      pure DeclarationAnalysis
        { daDeclarations = decls
        , daCategories = grouped
        , daDependencies = deps
        , daCurrentOrder = currentOrder
        , daSuggestedOrder = suggestedOrder
        , daIsWellOrdered = isWellOrdered
        }

-- | Empty analysis for error cases
emptyAnalysis :: DeclarationAnalysis
emptyAnalysis = DeclarationAnalysis
  { daDeclarations = []
  , daCategories = Map.empty
  , daDependencies = Map.empty
  , daCurrentOrder = []
  , daSuggestedOrder = []
  , daIsWellOrdered = True
  }

--------------------------------------------------------------------------------
-- Declaration Extraction
--------------------------------------------------------------------------------

-- | Extract all declarations from a module
extractDeclarations :: FilePath -> Text -> HsModule GhcPs -> [DeclarationInfo]
extractDeclarations file content hsmod =
  let lns = T.lines content
      rawDecls = mapMaybe (extractDeclInfo file lns) (hsmodDecls hsmod)
      -- Merge type signatures with their bindings
      (sigs, others) = partition ((== DCTypeSig) . diCategory) rawDecls
      merged = mergeSignaturesWithBindings sigs others
  in merged

-- | Extract info from a single declaration
extractDeclInfo :: FilePath -> [Text] -> LHsDecl GhcPs -> Maybe DeclarationInfo
extractDeclInfo file lns (L loc decl) =
  let srcSpan = spanToSrcSpan file (locA loc)
      startLine = srcSpanStartLineRaw srcSpan
      endLine = srcSpanEndLineRaw srcSpan
      declText = extractLines lns startLine endLine
      name = getDeclName decl
      category = categorizeDecl decl
      deps = getDeclDependencies decl
  in case name of
    Nothing -> Nothing
    Just n -> Just DeclarationInfo
      { diName = n
      , diCategory = category
      , diSpan = srcSpan
      , diStartLine = startLine
      , diEndLine = endLine
      , diText = declText
      , diDependencies = deps
      , diAssociatedSig = Nothing
      , diCommentBefore = extractCommentBefore lns startLine
      }

-- | Extract lines from source
extractLines :: [Text] -> Int -> Int -> Text
extractLines lns startLine endLine =
  T.unlines $ take (endLine - startLine + 1) $ drop (startLine - 1) lns

-- | Extract comment immediately before a declaration
extractCommentBefore :: [Text] -> Int -> Maybe Text
extractCommentBefore lns startLine
  | startLine <= 1 = Nothing
  | otherwise =
      let prevLines = reverse $ take (startLine - 1) lns
          commentLines = takeWhile isCommentOrBlank prevLines
      in if null commentLines || all T.null commentLines
         then Nothing
         else Just $ T.unlines $ reverse $ dropWhile T.null commentLines
  where
    isCommentOrBlank line =
      T.null (T.strip line) ||
      T.isPrefixOf "--" (T.stripStart line) ||
      T.isPrefixOf "{-" (T.stripStart line)

-- | Get the name of a declaration
getDeclName :: HsDecl GhcPs -> Maybe Text
getDeclName decl = case decl of
  TyClD _ tyDecl -> Just $ getTyClDeclName tyDecl
  InstD _ _ -> Just "(instance)"
  ValD _ bind -> getBindName bind
  SigD _ sig -> getSigName sig
  DerivD _ _ -> Just "(deriving)"
  DefD _ _ -> Just "(default)"
  ForD _ _ -> Just "(foreign)"
  _ -> Nothing

-- | Get name from a type/class declaration
getTyClDeclName :: TyClDecl GhcPs -> Text
getTyClDeclName decl = case decl of
  FamDecl _ (FamilyDecl {fdLName = L _ name}) -> rdrNameToText name
  SynDecl _ (L _ name) _ _ _ -> rdrNameToText name
  DataDecl _ (L _ name) _ _ _ -> rdrNameToText name
  ClassDecl {tcdLName = L _ name} -> rdrNameToText name

-- | Get name from a binding
getBindName :: HsBind GhcPs -> Maybe Text
getBindName bind = case bind of
  FunBind _ (L _ name) _ -> Just $ rdrNameToText name
  PatBind _ pat _ _ -> getPatName pat
  VarBind _ name _ -> Just $ rdrNameToText name
  _ -> Nothing

-- | Get name from a pattern
getPatName :: LPat GhcPs -> Maybe Text
getPatName (L _ pat) = case pat of
  VarPat _ (L _ name) -> Just $ rdrNameToText name
  _ -> Nothing

-- | Get name from a signature
getSigName :: Sig GhcPs -> Maybe Text
getSigName sig = case sig of
  TypeSig _ (L _ name:_) _ -> Just $ rdrNameToText name
  PatSynSig _ (L _ name:_) _ -> Just $ rdrNameToText name
  ClassOpSig _ _ (L _ name:_) _ -> Just $ rdrNameToText name
  _ -> Nothing

-- | Convert RdrName to Text
rdrNameToText :: RdrName -> Text
rdrNameToText rdr = T.pack $ occNameString $ rdrNameOcc rdr

-- | Categorize a declaration
categorizeDecl :: HsDecl GhcPs -> DeclCategory
categorizeDecl decl = case decl of
  TyClD _ tyDecl -> categorizeTyClDecl tyDecl
  InstD _ _ -> DCInstanceDecl
  ValD _ _ -> DCFunctionDecl
  SigD _ sig -> categorizeSig sig
  _ -> DCOtherDecl

-- | Categorize a type/class declaration
categorizeTyClDecl :: TyClDecl GhcPs -> DeclCategory
categorizeTyClDecl decl = case decl of
  FamDecl {} -> DCTypeDecl
  SynDecl {} -> DCTypeDecl
  DataDecl _ _ _ _ defn ->
    if isNewtype defn then DCNewtypeDecl else DCDataDecl
  ClassDecl {} -> DCClassDecl
  where
    isNewtype HsDataDefn { dd_cons = cons } =
      case cons of
        NewTypeCon _ -> True
        _ -> False
    isNewtype (XHsDataDefn _) = False

-- | Categorize a signature
categorizeSig :: Sig GhcPs -> DeclCategory
categorizeSig _ = DCTypeSig

-- | Get dependencies from a declaration
getDeclDependencies :: HsDecl GhcPs -> Set Text
getDeclDependencies decl = case decl of
  ValD _ bind -> getBindDependencies bind
  TyClD _ tyDecl -> getTyClDeclDependencies tyDecl
  InstD _ inst -> getInstDeclDependencies inst
  SigD _ sig -> getSigDependencies sig
  _ -> Set.empty

-- | Get dependencies from a binding
getBindDependencies :: HsBind GhcPs -> Set Text
getBindDependencies bind = case bind of
  FunBind _ _ mg -> getMatchGroupDependencies mg
  PatBind _ _ _ grhss -> getGRHSsDependencies grhss
  _ -> Set.empty

-- | Get dependencies from a match group
getMatchGroupDependencies :: MatchGroup GhcPs (LHsExpr GhcPs) -> Set Text
getMatchGroupDependencies (MG _ (L _ matches)) =
  Set.unions $ map getMatchDependencies matches

-- | Get dependencies from a match
getMatchDependencies :: LMatch GhcPs (LHsExpr GhcPs) -> Set Text
getMatchDependencies (L _ (Match _ _ _ grhss)) = getGRHSsDependencies grhss

-- | Get dependencies from GRHSs
getGRHSsDependencies :: GRHSs GhcPs (LHsExpr GhcPs) -> Set Text
getGRHSsDependencies (GRHSs _ grhss localBinds) =
  Set.unions (map getGRHSDependencies grhss) `Set.union`
  getLocalBindsDependencies localBinds

-- | Get dependencies from a GRHS
getGRHSDependencies :: LGRHS GhcPs (LHsExpr GhcPs) -> Set Text
getGRHSDependencies (L _ (GRHS _ _ expr)) = getExprDependencies expr

-- | Get dependencies from local bindings
getLocalBindsDependencies :: HsLocalBinds GhcPs -> Set Text
getLocalBindsDependencies localBinds = case localBinds of
  HsValBinds _ (ValBinds _ binds _) ->
    Set.unions $ map (getBindDependencies . unLoc) (bagToList binds)
  _ -> Set.empty

-- | Get dependencies from an expression
getExprDependencies :: LHsExpr GhcPs -> Set Text
getExprDependencies (L _ expr) = case expr of
  HsVar _ (L _ rdr) ->
    case rdr of
      Unqual occ -> Set.singleton $ T.pack $ occNameString occ
      _ -> Set.empty
  HsApp _ e1 e2 -> getExprDependencies e1 `Set.union` getExprDependencies e2
  HsAppType _ e _ -> getExprDependencies e
  HsLam _ _ mg -> getMatchGroupDependencies mg
  HsLet _ binds body ->
    getLocalBindsDependencies binds `Set.union` getExprDependencies body
  HsIf _ c t f ->
    getExprDependencies c `Set.union`
    getExprDependencies t `Set.union`
    getExprDependencies f
  HsCase _ scrut mg ->
    getExprDependencies scrut `Set.union` getMatchGroupDependencies mg
  HsDo _ _ (L _ stmts) -> Set.unions $ map getStmtDependencies stmts
  ExplicitList _ exprs -> Set.unions $ map getExprDependencies exprs
  ExplicitTuple _ args _ -> Set.unions $ mapMaybe getArgDependencies args
    where
      getArgDependencies (Present _ e) = Just $ getExprDependencies e
      getArgDependencies (Missing _) = Nothing
  HsPar _ e -> getExprDependencies e
  OpApp _ e1 op e2 ->
    getExprDependencies e1 `Set.union`
    getExprDependencies op `Set.union`
    getExprDependencies e2
  NegApp _ e _ -> getExprDependencies e
  SectionL _ e1 e2 -> getExprDependencies e1 `Set.union` getExprDependencies e2
  SectionR _ e1 e2 -> getExprDependencies e1 `Set.union` getExprDependencies e2
  _ -> Set.empty

-- | Get dependencies from a statement
getStmtDependencies :: LStmt GhcPs (LHsExpr GhcPs) -> Set Text
getStmtDependencies (L _ stmt) = case stmt of
  LastStmt _ e _ _ -> getExprDependencies e
  BindStmt _ _ e -> getExprDependencies e
  BodyStmt _ e _ _ -> getExprDependencies e
  LetStmt _ binds -> getLocalBindsDependencies binds
  _ -> Set.empty

-- | Get dependencies from a type/class declaration
getTyClDeclDependencies :: TyClDecl GhcPs -> Set Text
getTyClDeclDependencies _ = Set.empty  -- Simplified

-- | Get dependencies from an instance declaration
getInstDeclDependencies :: InstDecl GhcPs -> Set Text
getInstDeclDependencies inst = case inst of
  ClsInstD _ clsInst ->
    let binds = cid_binds clsInst
    in Set.unions $ map (getBindDependencies . unLoc) (bagToList binds)
  _ -> Set.empty

-- | Get dependencies from a signature
getSigDependencies :: Sig GhcPs -> Set Text
getSigDependencies _ = Set.empty  -- Type signatures don't create dependencies

--------------------------------------------------------------------------------
-- Sorting Strategies
--------------------------------------------------------------------------------

-- | Sort declarations alphabetically
sortAlphabetical :: [DeclarationInfo] -> [DeclarationInfo]
sortAlphabetical = sortBy (comparing diName)

-- | Sort with types first, then functions, then instances
sortTypeFirst :: [DeclarationInfo] -> [DeclarationInfo]
sortTypeFirst decls =
  let categoryOrder = [DCTypeSig, DCTypeDecl, DCDataDecl, DCNewtypeDecl,
                       DCClassDecl, DCFunctionDecl, DCInstanceDecl, DCOtherDecl]
      getOrder :: DeclCategory -> Int
      getOrder cat = fromMaybe maxBound $ lookup cat (zip categoryOrder [0..])
  in sortBy (comparing (getOrder . diCategory)) decls

-- | Sort by dependencies (topological sort)
sortDependency :: [DeclarationInfo] -> Map Text (Set Text) -> [DeclarationInfo]
sortDependency decls deps =
  let nameToDecl = Map.fromList [(diName d, d) | d <- decls]
      sorted = topologicalSort (map diName decls) deps
  in mapMaybe (`Map.lookup` nameToDecl) sorted

-- | Custom sort based on config
sortCustom :: SortConfig -> [DeclarationInfo] -> [DeclarationInfo]
sortCustom config decls =
  let categoryOrder = scCategoryOrder config
      getOrder :: DeclCategory -> Int
      getOrder cat = fromMaybe maxBound $ lookup cat (zip categoryOrder [0..])
  in sortBy (comparing (getOrder . diCategory)) decls

-- | Topological sort of names by dependencies
topologicalSort :: [Text] -> Map Text (Set Text) -> [Text]
topologicalSort names deps = go Set.empty names []
  where
    go _ [] acc = reverse acc
    go visited (n:ns) acc
      | n `Set.member` visited = go visited ns acc
      | otherwise =
          let myDeps = fromMaybe Set.empty $ Map.lookup n deps
              depList = filter (`elem` names) $ Set.toList myDeps
              (newVisited, depsSorted) = foldl processDep (visited, []) depList
          in go (Set.insert n newVisited) ns (n : depsSorted ++ acc)

    processDep (vis, acc) name
      | name `Set.member` vis = (vis, acc)
      | otherwise =
          let myDeps = fromMaybe Set.empty $ Map.lookup name deps
              depList = filter (`elem` names) $ Set.toList myDeps
              (newVis, depsSorted) = foldl processDep (vis, []) depList
          in (Set.insert name newVis, name : depsSorted ++ acc)

-- | Compute suggested order based on strategy
computeSuggestedOrder :: SortConfig -> [DeclarationInfo] -> Map Text (Set Text) -> [Text]
computeSuggestedOrder config decls deps =
  let sorted = case scStrategy config of
        Alphabetical -> sortAlphabetical decls
        TypeFirst -> sortTypeFirst decls
        DependencyOrder -> sortDependency decls deps
        CustomOrder -> sortCustom config decls
        NoSort -> decls
  in map diName sorted

--------------------------------------------------------------------------------
-- Grouping
--------------------------------------------------------------------------------

-- | Group declarations by category
groupByCategory :: [DeclarationInfo] -> Map DeclCategory [DeclarationInfo]
groupByCategory decls = Map.fromListWith (++)
  [(diCategory d, [d]) | d <- decls]

-- | Merge type signatures with their corresponding bindings
mergeSignaturesWithBindings :: [DeclarationInfo] -> [DeclarationInfo] -> [DeclarationInfo]
mergeSignaturesWithBindings sigs others =
  let sigMap = Map.fromList [(diName s, s) | s <- sigs]

      attachSig decl =
        case Map.lookup (diName decl) sigMap of
          Nothing -> decl
          Just sig -> decl { diAssociatedSig = Just (diSpan sig) }

      othersWithSigs = map attachSig others

      -- Keep orphan signatures (sigs without matching bindings)
      orphanSigs = filter (\s -> diName s `Set.notMember` Set.fromList (map diName others)) sigs
  in orphanSigs ++ othersWithSigs

-- | Build dependency map from declarations
buildDependencyMap :: [DeclarationInfo] -> Map Text (Set Text)
buildDependencyMap decls = Map.fromList
  [(diName d, diDependencies d) | d <- decls]

--------------------------------------------------------------------------------
-- Fix Generation
--------------------------------------------------------------------------------

-- | Generate a fix for sorting declarations
generateSortFix :: FilePath -> Text -> SortConfig -> DeclarationAnalysis -> Fix
generateSortFix _file content config analysis = Fix
  { fixTitle = "Sort declarations (" <> strategyName <> ")"
  , fixEdits = [FixEdit moduleBodySpan sortedContent]
  , fixIsPreferred = False
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSReview
  }
  where
    strategyName = case scStrategy config of
      Alphabetical -> "alphabetical"
      TypeFirst -> "type-first"
      DependencyOrder -> "dependency"
      CustomOrder -> "custom"
      NoSort -> "none"

    moduleBodySpan = computeModuleBodySpan content (daDeclarations analysis)
    sortedContent = applySort content analysis config

-- | Compute the span covering all declarations
computeModuleBodySpan :: Text -> [DeclarationInfo] -> SrcSpan
computeModuleBodySpan content decls
  | null decls = noSrcSpan
  | otherwise =
      let startLine = minimum $ map diStartLine decls
          endLine = maximum $ map diEndLine decls
          endCol = maybe 1 (T.length . (!! (endLine - 1))) $
                   if length (T.lines content) >= endLine
                   then Just (T.lines content)
                   else Nothing
      in mkSrcSpanRaw "" startLine 1 endLine endCol

-- | Apply sorting and generate new content
applySort :: Text -> DeclarationAnalysis -> SortConfig -> Text
applySort content analysis config =
  let decls = daDeclarations analysis
      sorted = sortDecls config decls (daDependencies analysis)
  in renderDeclarations config content sorted

-- | Sort declarations according to config
sortDecls :: SortConfig -> [DeclarationInfo] -> Map Text (Set Text) -> [DeclarationInfo]
sortDecls config decls deps = case scStrategy config of
  Alphabetical -> sortAlphabetical decls
  TypeFirst -> sortTypeFirst decls
  DependencyOrder -> sortDependency decls deps
  CustomOrder -> sortCustom config decls
  NoSort -> decls

-- | Render sorted declarations back to source code
renderDeclarations :: SortConfig -> Text -> [DeclarationInfo] -> Text
renderDeclarations config _originalContent decls =
  let grouped = if scGroupingStyle config == GroupByCategory
                then groupAndRenderByCategory config decls
                else map diText decls
      separator = T.replicate (scBlankLinesBetweenGroups config + 1) "\n"
  in T.intercalate separator grouped

-- | Group declarations by category and render
groupAndRenderByCategory :: SortConfig -> [DeclarationInfo] -> [Text]
groupAndRenderByCategory config decls =
  let categoryOrder = scCategoryOrder config
      grouped = groupByCategory decls
      renderGroup cat = case Map.lookup cat grouped of
        Nothing -> Nothing
        Just ds ->
          let sorted = if scSortWithinGroups config
                       then sortBy (comparing diName) ds
                       else ds
          in Just $ T.unlines $ map renderDeclWithComment sorted
  in catMaybes $ map renderGroup categoryOrder

-- | Render a declaration with its associated comment
renderDeclWithComment :: DeclarationInfo -> Text
renderDeclWithComment decl =
  let commentText = fromMaybe "" (diCommentBefore decl)
  in if T.null commentText
     then T.stripEnd (diText decl)
     else commentText <> diText decl

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Count how many declarations changed position
countChanges :: [Text] -> [Text] -> Int
countChanges original suggested =
  length $ filter (uncurry (/=)) $ zip original suggested
