module Main exposing (..)

{-|



-}

import Html exposing (text)
import Regex exposing (..)
import Debug exposing (log)
import String


type alias Matcher a = List (Maybe String) -> Block a

type alias Reverser a = Block a -> String
  

type Block a
  = Paragraph a
  | Header a


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


replaceBlock : Matcher a -> Reverser a -> Match -> String
replaceBlock matcher reverser { submatches } =
  submatches
    |> matcher
    |> reverser



replaceHeader =
  replaceBlock matchHeader reverseHeader


test : String
test = """
### 123

haaaahaaa

## header 2
axx

456
"""


viewHeader : Int -> String -> String
viewHeader num content =
  let
    n =
      toString num
  in
    "<h" ++ n ++ ">" ++ content ++ "</h" ++ n ++ ">"

defaultBlock : String -> String
defaultBlock content =
  "<p>" ++ content ++ "</p>"


reHeader : Regex
reHeader =
  regex "(#+)\\s*(.*)"


type alias Matching a =
  { flag : a -> Block a
  , matcher : Match -> String
  , regexp : Regex
  , result : Bool
  , match : String
  }

headerMatching : Matching a
headerMatching =
  { flag = Header
  , matcher = replaceBlock matchHeader reverseHeader
  , regexp = reHeader
  , result = False
  , match = ""
  }

        
matchers : List (Matching a)
matchers =
  [ headerMatching
  ]
          

splitBlocks : String -> List String
splitBlocks =
  Regex.split Regex.All <| regex "\\n{2}"
       

convertBlock : String -> String       
convertBlock str =
  let
    matching m =
      { m |
        result = Regex.contains m.regexp str
      , match = str
      }
    
    matched =
      List.map matching matchers

    filted =
      List.filter (\{ result } -> result) matched

    result =
      List.head filted

    replacer =
      Regex.replace Regex.All

    default =
      defaultBlock str

  in
    case result of
      Just { flag, matcher, regexp, match } ->
        case flag match of
          Header match ->
            replacer regexp matcher match
          _ ->
            default
      Nothing ->
        default


main =
  test
    |> String.trim
    |> splitBlocks
    |> List.map convertBlock
    |> toString
    |> text
