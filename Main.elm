{- mode: elm -}
{- coding: utf-8 -}
module Main exposing (..)

import Html exposing (text)
import Regex exposing (..)
import Debug exposing (log)
import String


type alias RegexMatch = Match -> String

type alias Matcher a = List (Maybe String) -> Block a

type alias Reverser a = Block a -> String

type alias Matching a =
  { flag : a -> Block a
  , matcher : RegexMatch
  , regexp : Regex
  , result : Bool
  , match : String
  }


type Block a
  = Paragraph
  | Header a
  | Blockquotes a


-- Header

reHeader : Regex
reHeader =
  regex "(#+)\\s*(.*)"

    
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



-- Blockquotes

reBlockquotes : Regex
reBlockquotes =
  regex ">\\s*(.*)"


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






        



-- Main Login

matchers : List (Matching a)
matchers =
  [ headerMatching
  , blockquotesMatching
  ]

replaceBlock : Matcher a -> Reverser a -> RegexMatch
replaceBlock matcher reverser { submatches } =
  submatches
    |> matcher
    |> reverser
         

defaultBlock : String -> String
defaultBlock content =
  "<p>" ++ content ++ "</p>"


          
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
        replacer regexp matcher match
      Nothing ->
        default



test : String
test = """
### 123

haaaahaaa

## header 2
axx

456

> 23333 what's fuck
"""          

main =
  test
    |> String.trim
    |> splitBlocks
    |> List.map convertBlock
    |> toString
    |> text
