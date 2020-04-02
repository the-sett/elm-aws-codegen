module UrlParser exposing (UrlPart(..), parseUrlParams)

{-| Provides a URL decoder for REST style URLs.

For example this URL specification `/user/{userId}` is saying that a user id
is expected in a certain position in the URL denoted by `{userId}`.

-}

import Parser exposing ((|.), (|=), Parser, Step(..), lazy, loop, oneOf, succeed, symbol, variable)
import Set exposing (Set)


{-| URLs like `/user/{userId}` can be broken down into two kinds of parts. Either a literal
part of the path like `/user/` or a parameter like `userId`.

A list of such parts when joined together with values substituted for the parameters will
result in a complete URL like `/user/123`.

-}
type UrlPart
    = PathLiteral String
    | Param String


parseUrlParams : String -> Result String (List UrlPart)
parseUrlParams path =
    Parser.run pathSpecParser path
        |> Result.mapError Parser.deadEndsToString


pathSpecParser : Parser (List UrlPart)
pathSpecParser =
    let
        step acc =
            oneOf
                [ succeed (\param -> Loop (param :: acc))
                    |. symbol "{"
                    |= paramParser
                    |. symbol "}"
                , succeed (\path -> Loop (path :: acc))
                    |= pathLiteralParser
                , succeed ()
                    |> Parser.map (\_ -> Done (List.reverse acc))
                ]
    in
    loop [] step


pathLiteralParser : Parser UrlPart
pathLiteralParser =
    let
        allowed c =
            Char.isAlphaNum c || c == '-' || c == '/'
    in
    variable
        { start = allowed
        , inner = allowed
        , reserved = Set.empty
        }
        |> Parser.map PathLiteral


paramParser : Parser UrlPart
paramParser =
    variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlpha c || c == '-'
        , reserved = Set.empty
        }
        |> Parser.map Param
