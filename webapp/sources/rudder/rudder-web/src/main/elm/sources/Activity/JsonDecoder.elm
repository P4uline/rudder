module Activity.JsonDecoder exposing (..)

import Activity.DataTypes exposing (..)
import Html exposing (Html, div)
import Json.Decode exposing (..)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (..)
import List exposing (drop, head)
import Markdown
import Markdown.Config exposing (Options)
import String exposing (join, split)
import Activity.HtmlStringAdapter exposing (text)


decodeGetActivities : Decoder (List Activity)
decodeGetActivities =
    at [ "data" ] (list decodeActivity)


decodeActivity : Decoder Activity
decodeActivity =
    succeed Activity
        |> required "id" int
        |> required "actor" string
        |> required "description" string
        |> required "date" Json.Decode.Extra.datetime

{-stringToHtmlDescription : Decoder (HtmlDescription msg)
stringToHtmlDescription =
    let
        string2HtmlMsg : String -> Decoder (HtmlDescription msg)
        string2HtmlMsg s = succeed (div[] (Markdown.toHtml Nothing s))
    in
        string |> andThen string2HtmlMsg-}

decodeErrorDetails : String -> ( String, String )
decodeErrorDetails json =
    let
        errorMsg =
            decodeString (Json.Decode.at [ "errorDetails" ] string) json

        msg =
            case errorMsg of
                Ok s ->
                    s

                Err _ ->
                    "fail to process errorDetails"

        errors =
            split "<-" msg

        title =
            head errors
    in
    case title of
        Nothing ->
            ( "", "" )

        Just s ->
            ( s, join " \n " (drop 1 (List.map (\err -> "\t ‣ " ++ err) errors)) )
