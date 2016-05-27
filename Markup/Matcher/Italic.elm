{- mode: elm -}
{- coding: utf-8 -}
module Markup.Matcher.Italic exposing (italicMatching)

import Markup.Type exposing (..)
import Markup.Action exposing (replaceBlock)
import Regex exposing (Regex)


{-|
*1*
**
*1
1*
*_1_*
-}

reItalic : Regex
reItalic =
  Regex.regex "_([^_|.]+)_"


matchItalic : Matcher (Maybe String)
matchItalic bq =
  case bq of
    content::[] ->
      Italic content
    _ ->
      Italic Nothing

reverseItalic : Reverser (Maybe String)
reverseItalic bq =
  case bq of
    Italic (Just content) ->
      viewItalic content
    _ ->
      ""

viewItalic : String -> String
viewItalic content =
  --"<span class=\"markup-inline markup-b\">" ++ content ++ "</span class=\"markup-inline markup-b\">"
  "<em>" ++ content ++ "</em>"


italicMatching : Matching a
italicMatching =
  { flag = Italic
  , matcher = replaceBlock matchItalic reverseItalic
  , regexp = reItalic
  , result = False
  , match = ""
  }
