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
extract file ((TypeSig srcSpan1 names ty) : f@(FunBind srcSpan2 funcBind) : xs) =
  catMaybes (fmap (toFunction funcBind) names) ++ extract file xs
  where
    getFuncTypes :: [Text] =
      dropLast $ splitFunctionArgs (pack (Haskell.prettyPrint ty))
    toFunction ((Match _ _ vars _ _) : _) n =
      Just
        ( Function
            { funcName = pack (Haskell.prettyPrint n),
              -- FIXME pass lineNumber from arg
              typeSignatureString = pack (Haskell.prettyPrint ty),
              -- FIXME pass lineNumber from arg + type
              functionArguments =
                fmap (\(t, (i, v)) -> FunctionArgument t v i) $
                  zip getFuncTypes (fmap (\v -> (getPatStartPos v, pack (Haskell.prettyPrint v))) vars),
              functionArgumentsString = Text.intercalate " " $ fmap (pack . Haskell.prettyPrint) vars,
              functionLineNumbers =
                [ (Haskell.srcSpanStartLine (Haskell.srcInfoSpan srcSpan1))
                  .. Haskell.srcSpanEndLine (Haskell.srcInfoSpan srcSpan2)
                ],
              functionBodyLines =
                Text.splitOn "\n" . Text.strip
                  . drop 1
                  $ snd (Text.breakOn "=" (pack (Haskell.prettyPrint f)))
            }
        )
    toFunction _ _ = Nothing
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
