module NestedSource where

-- Function with nested entity
nestedFunc ::
  Entity Page ->
  Entity User ->
  Text
nestedFunc pageEntity (Entity _ uV) = "Test"
