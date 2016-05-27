{- mode: elm -}
{- coding: utf-8 -}
module Markup.Action exposing (replaceBlock)

import Markup.Type exposing (Matcher, Reverser, RegexMatch)


replaceBlock : Matcher b -> Reverser b -> RegexMatch
replaceBlock matcher reverser { submatches } =
  submatches
    |> matcher
    |> reverser
