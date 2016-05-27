{- mode: elm -}
{- coding: utf-8 -}
module Markup.Matcher.Blockquotes exposing (blockquotesMatching)

import Markup.Type exposing (..)
import Markup.Action exposing (replaceBlock)
import Regex exposing (Regex)


reBlockquotes : Regex
reBlockquotes =
  Regex.regex ">\\s*(.*)"


matchBlockquotes : Matcher (Maybe String)
matchBlockquotes bq =
  case bq of
    content::[] ->
      Blockquotes content
    _ ->
      Blockquotes Nothing

reverseBlockquotes : Reverser (Maybe String)
reverseBlockquotes bq =
  case bq of
    Blockquotes (Just content) ->
      viewBlockquotes content
    _ ->
      ""

viewBlockquotes : String -> String
viewBlockquotes content =
  "<blockquote>" ++ content ++ "</blockquote>"


blockquotesMatching : Matching a
blockquotesMatching =
  { flag = Blockquotes
  , matcher = replaceBlock matchBlockquotes reverseBlockquotes
  , regexp = reBlockquotes
  , result = False
  , match = ""
  }
