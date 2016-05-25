module Main exposing (..)

import Html exposing (..)
import Regex exposing (..)
import Debug exposing (log)
import String

type Block a
  = Header a
  | Paragraph


flatmap : Maybe (Maybe a) -> Maybe a
flatmap maybe =
  let
    _ =
      log "maybe" maybe
  in
    case maybe of
      Just (Just a) ->
        Just a
      _ ->
        Nothing


makeHeader : List (Maybe String) -> Block (Maybe Int, Maybe String)
makeHeader header =
  let
    (num, content) =
      case header of
        num::content::xs -> (num, content)
        _ -> (Nothing, Nothing)
  in
    Header (Maybe.map String.length num, content)

reverseHeader : Block (Maybe Int, Maybe String) -> String
reverseHeader header =
  case header of
    Header (num, content) ->
      "h2 aa"
    _ ->
      ""


reHeader : Regex
reHeader =
  regex "^(\\*+)\\s*(.*)"

defaultHeader : Int
defaultHeader =
  1

defaultContent : String
defaultContent =
  ""

    

replaceHeader : Match -> String
replaceHeader { match, submatches, index } =
  let
    _ =
      log "match" match
    _ =
      log "submatches" (reverseHeader <| makeHeader submatches) |> toString
    _ =
      log "index" index |> toString

  in
    match

reList : Regex
reList =
  regex ""


test : String
test = """
* 123
"""


main =
  toString >> text <| replace All reHeader replaceHeader "** 123asdf"
