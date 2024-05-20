module Test4 where

-- LocationId replaced in type signature not in comment
-- PageId replaced in type signature not in comment
func1 ::
  Key Location ->
  Key Page ->
  Text
func1 locationK pageK = "Test"

-- UserId replaced in type signature not in comment
-- PostId replaced in type signature not in comment
func2 ::
  Key User ->
  Key Post ->
  Text
func2 userK postK = "Test"
