{- mode: elm -}
{- coding: utf-8 -}
module Markup.Matcher.Link exposing (linkMatching)

import Markup.Type exposing (..)
import Markup.Action exposing (replaceBlock)
import Regex exposing (Regex)


reLink : Regex
reLink =
  Regex.regex "\\[(.*)\\]\\((.*)\\)"


matchLink : Matcher (Maybe String, Maybe String)
matchLink bq =
  case bq of
    content::href::[] ->
      {-
      case href of
        "" ->
          Link (content, Nothing)
        _ ->
          Link (content, href)
       -}
      Link (content, href)
    _ ->
      Link (Nothing, Nothing)

reverseLink : Reverser (Maybe String, Maybe String)
reverseLink bq =
  case bq of
    Link (Just content, Just href) ->
      viewLink content href
    Link (Just content, Nothing) ->
      viewLink content ""
    _ ->
      ""

viewLink : String -> String -> String
viewLink content href =
  "<a href=\"" ++ href ++ "\">" ++ content ++ "</a>"


linkMatching : Matching a
linkMatching =
  { flag = Link
  , matcher = replaceBlock matchLink reverseLink
  , regexp = reLink
  , result = False
  , match = ""
  }
