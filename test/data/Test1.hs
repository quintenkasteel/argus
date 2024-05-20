module Test1 where

check ::
  Text ->
  LocationId ->
  (a -> b) ->
  Entity Page ->
  Text
check arg1 _loca _ (Entity _ page) = "" >> ""
