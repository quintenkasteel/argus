module Test6 where

-- Function with LocationId and PageId
func1 ::
  Key Location ->
  Key Page ->
  Text
func1 locationK pageK = "Test"

-- Function with UserId and PostId
func2 ::
  Key User ->
  Key Post ->
  Text
func2 userK postK = "Test"
