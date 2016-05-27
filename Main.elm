{- mode: elm -}
{- coding: utf-8 -}
module Main exposing (..)

import Html exposing (text)
import Regex exposing (..)
import Debug exposing (log)
import String

import Markup.Type exposing (..)
import Markup.Matcher.Header exposing (headerMatching)
import Markup.Matcher.Blockquotes exposing (blockquotesMatching)
import Markup.Matcher.Bold exposing (boldMatching)
import Markup.Matcher.Italic exposing (italicMatching)
import Markup.Matcher.Code exposing (codeMatching)
import Markup.Matcher.Link exposing (linkMatching)


-- Main Login

blockMatchers : List (Matching a)
blockMatchers =
  [ headerMatching
  , blockquotesMatching
  ]
         

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
      List.map matching blockMatchers

    filted =
      List.filter (\{ result } -> result) matched

    result =
      List.head filted

    replacer =
      Regex.replace Regex.All

    default =
      --defaultBlock str
      str
        |> replacer boldMatching.regexp boldMatching.matcher
        |> replacer italicMatching.regexp italicMatching.matcher
        |> replacer codeMatching.regexp codeMatching.matcher
        |> replacer linkMatching.regexp linkMatching.matcher

  in
    case result of
      Just { flag, matcher, regexp, match } ->
        replacer regexp matcher match
      Nothing ->
        default



test : String
test = """
### 123 {#iddd}

ha**aaa**ha_aa_cccccc[c](http://www.baidu.com)ccdddddddd
asd~fasdf~asdfopqwie[ttt]()
asdfpoiq[23333]()

[baidu]()

23321asdlj,zmnxvciopwqerkljnlkasdf
asdfipowqerpoi;lajs ldkfj

[baidu]: http://www.baidu.com

wow`fuckit`wwwwwwwwww![233](img/233)wwww

  function() {
    
  }

```js
function() {
  console.log("23333")
}
```

| name | age | page |
| ---- | --= | -+-= |
| mama | 123 | 111  |
| baba | 456 | 222  |

* asdasd
* 123123
  * lii
+ 123
- aspipqwe
1. qwe
2. 23543245

## header 2
axx

`456asd`ad

> 23333 what's `fuck`
"""

main =
  let
    _ =
      log "test" <| Regex.replace Regex.All boldMatching.regexp boldMatching.matcher "233*1*123asd_asasdf*2*"
  in
    test
      |> String.trim
      |> splitBlocks
      |> List.map convertBlock
      --|> String.join ""
      |> toString
      |> text
