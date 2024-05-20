module NestedSource where

-- Function with nested entity
nestedFunc ::
  Entity Page ->
  Entity (Entity User) ->
  Text
nestedFunc page user = "Test"
