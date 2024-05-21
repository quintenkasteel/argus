{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Extra.Function (extract, getArgByType) where

import ClassyPrelude
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import Function (Function (..), FunctionArgument (..))
import Language.Haskell.Exts (Decl (..), Match (..), Pat (..), SrcSpanInfo)
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
                  fmap (\(t, (i, v)) -> FunctionArgument t v i) $
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
    getFunctionArguments :: [Match SrcSpanInfo] -> (Text, [(Maybe Int, Text)])
    getFunctionArguments (m@(Match _ _ vars _ _) : _) =
      ( pack (Haskell.prettyPrint m),
        fmap (\v -> (getPatStartPos v, pack (Haskell.prettyPrint v))) vars
      )
    getFunctionArguments _ = ("", [])
extract _ _ = []

getArgByType :: Text -> [FunctionArgument] -> Maybe FunctionArgument
getArgByType typeArg = find (\(FunctionArgument {type_}) -> typeArg == type_ || [i|(#{typeArg})|] == type_)

splitFunctionArgs :: Text -> [Text]
splitFunctionArgs = fmap (Text.strip . pack) . Util.splitRespectingParens "->" . unpack

-- Function to extract start position for each constructor of Pat
getPatStartPos :: Pat SrcSpanInfo -> Maybe Int
getPatStartPos (PVar srcSpanInfo _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PLit srcSpanInfo _ _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PInfixApp srcSpanInfo _ _ _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PApp srcSpanInfo _ _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PTuple srcSpanInfo _ _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PList srcSpanInfo _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PParen srcSpanInfo _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PRec srcSpanInfo _ _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PAsPat srcSpanInfo _ _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PWildCard srcSpanInfo) = Just (fromInfo srcSpanInfo)
getPatStartPos (PIrrPat srcSpanInfo _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PatTypeSig srcSpanInfo _ _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PViewPat srcSpanInfo _ _) = Just (fromInfo srcSpanInfo)
getPatStartPos (PBangPat srcSpanInfo _) = Just (fromInfo srcSpanInfo)
getPatStartPos _ = Nothing

fromInfo :: SrcSpanInfo -> Int
fromInfo =
  Haskell.srcSpanStartColumn . Haskell.srcInfoSpan
