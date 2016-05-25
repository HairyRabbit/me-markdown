module Main exposing (..)

import Html exposing (..)
import Regex exposing (..)
import Debug exposing (log)
import String

type Block a
  = Header a
  | Paragraph


makeHeader : List (Maybe String) -> Block (Maybe Int, Maybe String)
makeHeader header =
  let
    (num, content) =
      case header of
        num1::content1::xs -> (num1, content1)
        _ -> (Just defaultContent, Just defaultContent)
  in
    Header (Maybe.map (String.toInt >> Result.toMaybe >> Maybe.withDefault defaultHeader) num, content)


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
      log "submatches" (makeHeader submatches) |> toString
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
