{- mode: elm -}
{- coding: utf-8 -}
module Markup.Matcher.Code exposing (codeMatching)

import Markup.Type exposing (..)
import Markup.Action exposing (replaceBlock)
import Regex exposing (Regex)


reCode : Regex
reCode =
  Regex.regex "\\`([^\\`|.]+)\\`"


matchCode : Matcher (Maybe String)
matchCode bq =
  case bq of
    content::[] ->
      Code content
    _ ->
      Code Nothing

reverseCode : Reverser (Maybe String)
reverseCode bq =
  case bq of
    Code (Just content) ->
      viewCode content
    _ ->
      ""

viewCode : String -> String
viewCode content =
  --"<span class=\"markup-inline markup-b\">" ++ content ++ "</span class=\"markup-inline markup-b\">"
  "<code>" ++ content ++ "</code>"


codeMatching : Matching a
codeMatching =
  { flag = Code
  , matcher = replaceBlock matchCode reverseCode
  , regexp = reCode
  , result = False
  , match = ""
  }
