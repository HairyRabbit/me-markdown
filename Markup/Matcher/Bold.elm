{- mode: elm -}
{- coding: utf-8 -}
module Markup.Matcher.Bold exposing (boldMatching)

import Markup.Type exposing (..)
import Markup.Action exposing (replaceBlock)
import Regex exposing (Regex)



reBold : Regex
reBold =
  Regex.regex "\\*([^\\*|.]+)\\*"


matchBold : Matcher (Maybe String)
matchBold bq =
  case bq of
    content::[] ->
      Bold content
    _ ->
      Bold Nothing

reverseBold : Reverser (Maybe String)
reverseBold bq =
  case bq of
    Bold (Just content) ->
      viewBold content
    _ ->
      ""

viewBold : String -> String
viewBold content =
  --"<span class=\"markup-inline markup-b\">" ++ content ++ "</span class=\"markup-inline markup-b\">"
  "<strong>" ++ content ++ "</strong>"


boldMatching : Matching a
boldMatching =
  { flag = Bold
  , matcher = replaceBlock matchBold reverseBold
  , regexp = reBold
  , result = False
  , match = ""
  }
