module Page where

type PageId = Int

data Page = Page
  { pageId :: PageId,
    pageName :: String
  }
