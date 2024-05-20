module Test6 where

-- Function with LocationId and PageId
func1 ::
  LocationId ->
  Key Page ->
  Text
func1 loc page = "Test"

-- Function with UserId and PostId
func2 ::
  UserId ->
  Key Post ->
  Text
func2 user post = "Test"
