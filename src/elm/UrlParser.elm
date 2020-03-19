module UrlParser exposing (parseUrlParams)

{-| Provides a URL decoder for REST style URLs.

For example this URL specification `/user/{userId}` is saying that a user id
is expected in a certain position in the URL denoted by `{userId}`.

-}

import Parser exposing ((|.), (|=), Parser, Step(..), lazy, loop, oneOf, succeed, symbol, variable)
import Set exposing (Set)


parseUrlParams : String -> Result String (List String)
parseUrlParams path =
    Parser.run pathSpecParser path
        |> Result.mapError Parser.deadEndsToString


pathSpecParser : Parser (List String)
pathSpecParser =
    let
        step acc =
            oneOf
                [ succeed (\param -> Loop (param :: acc))
                    |. symbol "{"
                    |= pathSegmentParser
                    |. symbol "}"
                , succeed (Loop acc)
                    |. pathSegmentParser
                , succeed ()
                    |> Parser.map (\_ -> Done (List.reverse acc))
                ]
    in
    loop [] step


pathSegmentParser : Parser String
pathSegmentParser =
    variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlpha c || c == '-'
        , reserved = Set.empty
        }
