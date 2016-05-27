{- mode: elm -}
{- coding: utf-8 -}
module Markup.Matcher.Header exposing (headerMatching)

import Markup.Type exposing (..)
import Markup.Action exposing (replaceBlock)
import Regex exposing (Regex)
import String

reHeader : Regex
reHeader =
  Regex.regex "(#+)\\s*(.*)"

    
matchHeader : Matcher (Maybe Int, Maybe String)
matchHeader header =
  case header of
    num::content::[] ->
      Header (Maybe.map String.length num, content)
    _ ->
      Header (Nothing, Nothing)


reverseHeader : Reverser (Maybe Int, Maybe String)
reverseHeader header =
  case header of
    Header (Just num, Just content) ->
      viewHeader num content
    _ ->
      ""

viewHeader : Int -> String -> String
viewHeader num content =
  let
    n =
      toString num
  in
    "<h" ++ n ++ ">" ++ content ++ "</h" ++ n ++ ">"


headerMatching : Matching a
headerMatching =
  { flag = Header
  , matcher = replaceBlock matchHeader reverseHeader
  , regexp = reHeader
  , result = False
  , match = ""
  }
