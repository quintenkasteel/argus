module Test1 where

check ::
  Text ->
  Key Location ->
  (a -> b) ->
  Entity Page ->
  Text
check arg1 _locationK _ (Entity _ pV) = "" >> ""
