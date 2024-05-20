{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Extra.Function (extract, getArgByType) where

import ClassyPrelude
import Data.String.Interpolate (i)
import Function (Function (..), FunctionArgument (..))
import Language.Haskell.Exts (Decl (..), Match (..), SrcSpanInfo)
import qualified Language.Haskell.Exts as Haskell
import Util

extract :: FilePath -> [Decl SrcSpanInfo] -> [Function]
extract f ((TypeSig srcSpan1 names ty) : (FunBind srcSpan2 funcBind) : xs) =
  func ++ extract f xs
  where
    func =
      fmap
        ( \n ->
            Function
              { funcName = pack (Haskell.prettyPrint n),
                typeSignatureString = pack (Haskell.prettyPrint ty),
                functionArguments =
                  fmap (\(t, v) -> FunctionArgument t v) $
                    zip getFuncTypes (snd (getFunctionArguments funcBind)),
                functionArgumentsString = fst (getFunctionArguments funcBind),
                functionLineNumbers =
                  [ (Haskell.srcSpanStartLine (Haskell.srcInfoSpan srcSpan1))
                    .. Haskell.srcSpanEndLine (Haskell.srcInfoSpan srcSpan2)
                  ]
              }
        )
        names
    getFuncTypes :: [Text] =
      dropLast $ splitFunctionArgs (pack (Haskell.prettyPrint ty))
    getFunctionArguments :: [Match SrcSpanInfo] -> (Text, [Text])
    getFunctionArguments (m@(Match _ _ vars _ _) : _) =
      (pack (Haskell.prettyPrint m), fmap (pack . Haskell.prettyPrint) vars)
    getFunctionArguments _ = ("", [])
extract _ _ = []

getArgByType :: Text -> [FunctionArgument] -> Maybe FunctionArgument
getArgByType typeArg = find (\(FunctionArgument {type_}) -> typeArg == type_ || [i|(#{typeArg})|] == type_)
