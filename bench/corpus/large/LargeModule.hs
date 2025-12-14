{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Large module for stress testing (~500 lines)
module LargeModule where

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Entity = Entity
  { entityId :: Int
  , entityName :: Text
  , entityType :: EntityType
  , entityStatus :: Status
  , entityMetadata :: Map Text Text
  }

data EntityType
  = TypeA
  | TypeB
  | TypeC
  | TypeD
  | TypeE
  deriving (Eq, Show, Ord)

data Status
  = Active
  | Inactive
  | Pending
  | Deleted
  | Archived
  deriving (Eq, Show, Ord)

data Operation
  = Create Entity
  | Update Entity
  | Delete Int
  | Archive Int
  | Restore Int

data Result a
  = Success a
  | Failure Text
  | Pending'
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Entity Operations
--------------------------------------------------------------------------------

createEntity :: Int -> Text -> EntityType -> Entity
createEntity eid name etype = Entity
  { entityId = eid
  , entityName = name
  , entityType = etype
  , entityStatus = Pending
  , entityMetadata = Map.empty
  }

updateEntity :: Entity -> Text -> Entity
updateEntity entity newName = entity { entityName = newName }

activateEntity :: Entity -> Entity
activateEntity entity = entity { entityStatus = Active }

deactivateEntity :: Entity -> Entity
deactivateEntity entity = entity { entityStatus = Inactive }

deleteEntity :: Entity -> Entity
deleteEntity entity = entity { entityStatus = Deleted }

archiveEntity :: Entity -> Entity
archiveEntity entity = entity { entityStatus = Archived }

--------------------------------------------------------------------------------
-- Collection Operations
--------------------------------------------------------------------------------

findById :: Int -> [Entity] -> Maybe Entity
findById eid = find (\e -> entityId e == eid)

findByName :: Text -> [Entity] -> [Entity]
findByName name = filter (\e -> entityName e == name)

findByType :: EntityType -> [Entity] -> [Entity]
findByType etype = filter (\e -> entityType e == etype)

findByStatus :: Status -> [Entity] -> [Entity]
findByStatus status = filter (\e -> entityStatus e == status)

findActive :: [Entity] -> [Entity]
findActive = findByStatus Active

findInactive :: [Entity] -> [Entity]
findInactive = findByStatus Inactive

findPending :: [Entity] -> [Entity]
findPending = findByStatus Pending

findDeleted :: [Entity] -> [Entity]
findDeleted = findByStatus Deleted

findArchived :: [Entity] -> [Entity]
findArchived = findByStatus Archived

--------------------------------------------------------------------------------
-- Grouping and Aggregation
--------------------------------------------------------------------------------

groupByType :: [Entity] -> Map EntityType [Entity]
groupByType = foldr insertByType Map.empty
  where
    insertByType e = Map.insertWith (++) (entityType e) [e]

groupByStatus :: [Entity] -> Map Status [Entity]
groupByStatus = foldr insertByStatus Map.empty
  where
    insertByStatus e = Map.insertWith (++) (entityStatus e) [e]

countByType :: [Entity] -> Map EntityType Int
countByType = Map.map length . groupByType

countByStatus :: [Entity] -> Map Status Int
countByStatus = Map.map length . groupByStatus

totalCount :: [Entity] -> Int
totalCount = length

activeCount :: [Entity] -> Int
activeCount = length . findActive

inactiveCount :: [Entity] -> Int
inactiveCount = length . findInactive

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateEntity :: Entity -> Either Text Entity
validateEntity entity@Entity{..}
  | entityId < 0 = Left "Invalid ID: must be non-negative"
  | T.null entityName = Left "Invalid name: cannot be empty"
  | T.length entityName > 255 = Left "Invalid name: too long"
  | otherwise = Right entity

validateEntities :: [Entity] -> ([Entity], [Text])
validateEntities = foldr go ([], [])
  where
    go entity (valid, errors) = case validateEntity entity of
      Right e -> (e : valid, errors)
      Left err -> (valid, err : errors)

isValidEntity :: Entity -> Bool
isValidEntity = either (const False) (const True) . validateEntity

allValid :: [Entity] -> Bool
allValid = all isValidEntity

anyInvalid :: [Entity] -> Bool
anyInvalid = any (not . isValidEntity)

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

mapEntities :: (Entity -> Entity) -> [Entity] -> [Entity]
mapEntities = map

filterEntities :: (Entity -> Bool) -> [Entity] -> [Entity]
filterEntities = filter

sortById :: [Entity] -> [Entity]
sortById = sortBy (compare `on` entityId)
  where on f g x y = f (g x) (g y)

sortByName :: [Entity] -> [Entity]
sortByName = sortBy (compare `on` entityName)
  where on f g x y = f (g x) (g y)

sortByType :: [Entity] -> [Entity]
sortByType = sortBy (compare `on` entityType)
  where on f g x y = f (g x) (g y)

sortByStatus :: [Entity] -> [Entity]
sortByStatus = sortBy (compare `on` entityStatus)
  where on f g x y = f (g x) (g y)

reverseEntities :: [Entity] -> [Entity]
reverseEntities = reverse

takeEntities :: Int -> [Entity] -> [Entity]
takeEntities = take

dropEntities :: Int -> [Entity] -> [Entity]
dropEntities = drop

--------------------------------------------------------------------------------
-- Partial Function Usage (for testing detection)
--------------------------------------------------------------------------------

getFirst :: [Entity] -> Entity
getFirst = head

getLast :: [Entity] -> Entity
getLast = last

getRest :: [Entity] -> [Entity]
getRest = tail

getInit :: [Entity] -> [Entity]
getInit = init

getAt :: Int -> [Entity] -> Entity
getAt i es = es !! i

getFirstOrError :: [Entity] -> Entity
getFirstOrError es
  | null es = error "Empty list"
  | otherwise = head es

unsafeExtract :: Maybe Entity -> Entity
unsafeExtract = fromJust

unsafeMax :: [Entity] -> Entity
unsafeMax es = maximumBy (compare `on` entityId) es
  where on f g x y = f (g x) (g y)

unsafeMin :: [Entity] -> Entity
unsafeMin es = minimumBy (compare `on` entityId) es
  where on f g x y = f (g x) (g y)

--------------------------------------------------------------------------------
-- Complex Operations
--------------------------------------------------------------------------------

processOperations :: [Operation] -> [Entity] -> [Entity]
processOperations ops entities = foldl' applyOp entities ops
  where
    applyOp es (Create e) = e : es
    applyOp es (Update e) = map (\x -> if entityId x == entityId e then e else x) es
    applyOp es (Delete eid) = map (\x -> if entityId x == eid then deleteEntity x else x) es
    applyOp es (Archive eid) = map (\x -> if entityId x == eid then archiveEntity x else x) es
    applyOp es (Restore eid) = map (\x -> if entityId x == eid then activateEntity x else x) es

batchUpdate :: (Entity -> Entity) -> [Int] -> [Entity] -> [Entity]
batchUpdate f ids entities = map update entities
  where
    idSet = Set.fromList ids
    update e = if entityId e `Set.member` idSet then f e else e

batchDelete :: [Int] -> [Entity] -> [Entity]
batchDelete = batchUpdate deleteEntity

batchArchive :: [Int] -> [Entity] -> [Entity]
batchArchive = batchUpdate archiveEntity

batchActivate :: [Int] -> [Entity] -> [Entity]
batchActivate = batchUpdate activateEntity

batchDeactivate :: [Int] -> [Entity] -> [Entity]
batchDeactivate = batchUpdate deactivateEntity

--------------------------------------------------------------------------------
-- Metadata Operations
--------------------------------------------------------------------------------

getMetadata :: Text -> Entity -> Maybe Text
getMetadata key Entity{..} = Map.lookup key entityMetadata

setMetadata :: Text -> Text -> Entity -> Entity
setMetadata key val entity = entity
  { entityMetadata = Map.insert key val (entityMetadata entity) }

removeMetadata :: Text -> Entity -> Entity
removeMetadata key entity = entity
  { entityMetadata = Map.delete key (entityMetadata entity) }

hasMetadata :: Text -> Entity -> Bool
hasMetadata key Entity{..} = Map.member key entityMetadata

metadataKeys :: Entity -> [Text]
metadataKeys Entity{..} = Map.keys entityMetadata

metadataValues :: Entity -> [Text]
metadataValues Entity{..} = Map.elems entityMetadata

metadataCount :: Entity -> Int
metadataCount Entity{..} = Map.size entityMetadata

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

entityStats :: [Entity] -> Map Text Int
entityStats entities = Map.fromList
  [ ("total", totalCount entities)
  , ("active", activeCount entities)
  , ("inactive", inactiveCount entities)
  , ("pending", length $ findPending entities)
  , ("deleted", length $ findDeleted entities)
  , ("archived", length $ findArchived entities)
  , ("typeA", length $ findByType TypeA entities)
  , ("typeB", length $ findByType TypeB entities)
  , ("typeC", length $ findByType TypeC entities)
  , ("typeD", length $ findByType TypeD entities)
  , ("typeE", length $ findByType TypeE entities)
  ]

summarizeEntities :: [Entity] -> Text
summarizeEntities entities = T.unlines
  [ "Entity Summary"
  , "=============="
  , "Total: " <> T.pack (show $ totalCount entities)
  , "Active: " <> T.pack (show $ activeCount entities)
  , "Inactive: " <> T.pack (show $ inactiveCount entities)
  , "By Type:"
  , "  TypeA: " <> T.pack (show $ length $ findByType TypeA entities)
  , "  TypeB: " <> T.pack (show $ length $ findByType TypeB entities)
  , "  TypeC: " <> T.pack (show $ length $ findByType TypeC entities)
  , "  TypeD: " <> T.pack (show $ length $ findByType TypeD entities)
  , "  TypeE: " <> T.pack (show $ length $ findByType TypeE entities)
  ]

--------------------------------------------------------------------------------
-- Additional Functions for Line Count
--------------------------------------------------------------------------------

func1 :: [Entity] -> Int
func1 = length . findActive

func2 :: [Entity] -> Int
func2 = length . findInactive

func3 :: [Entity] -> Int
func3 = length . findPending

func4 :: [Entity] -> Int
func4 = length . findDeleted

func5 :: [Entity] -> Int
func5 = length . findArchived

func6 :: [Entity] -> Bool
func6 = null

func7 :: [Entity] -> Bool
func7 = not . null

func8 :: [Entity] -> Maybe Entity
func8 [] = Nothing
func8 (x:_) = Just x

func9 :: [Entity] -> Maybe Entity
func9 [] = Nothing
func9 xs = Just (last xs)

func10 :: [Entity] -> [Entity]
func10 = take 10

func11 :: [Entity] -> [Entity]
func11 = drop 10

func12 :: [Entity] -> [Entity]
func12 = reverse

func13 :: Int -> [Entity] -> [Entity]
func13 = take

func14 :: Int -> [Entity] -> [Entity]
func14 = drop

func15 :: [Entity] -> [Int]
func15 = map entityId

func16 :: [Entity] -> [Text]
func16 = map entityName

func17 :: [Entity] -> [EntityType]
func17 = map entityType

func18 :: [Entity] -> [Status]
func18 = map entityStatus

func19 :: [Entity] -> Set Int
func19 = Set.fromList . map entityId

func20 :: [Entity] -> Set Text
func20 = Set.fromList . map entityName
