{- mode: elm -}
{- coding: utf-8 -}
module Markup.Type exposing (..)


import Regex exposing (Regex, Match)


type alias RegexMatch = Match -> String


type alias Matcher a = List (Maybe String) -> Flag a
type alias Reverser a = Flag a -> String


type alias Matching a =
  { flag : a -> Flag a
  , matcher : RegexMatch
  , regexp : Regex
  , result : Bool
  , match : String
  }


type Flag a
  = Paragraph
  | Header a
  | Blockquotes a
  | Bold a
  | Italic a
  | Code a
  | Link a
