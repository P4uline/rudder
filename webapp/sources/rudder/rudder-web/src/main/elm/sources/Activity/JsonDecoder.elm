module Activity.JsonDecoder exposing (..)

import Activity.DataTypes exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (..)
import List exposing (drop, head)
import String exposing (join, split)
import Activity.HtmlStringAdapter exposing (text)


decodeGetActivities : Decoder (List (Activity ActivityMsg))
decodeGetActivities =
    at [ "data" ] (list decodeActivity)


decodeActivity : Decoder (Activity ActivityMsg)
decodeActivity =
    succeed Activity
        |> required "id" int
        |> required "actor" string
        |> required "description" stringToHtmlDescription
        |> required "description" string
        |> required "date" Json.Decode.Extra.datetime

stringToHtmlDescription : Decoder (HtmlDescription msg)
stringToHtmlDescription =
    let
        s2HtmlMsg : String -> Decoder (HtmlDescription msg)
        s2HtmlMsg s = succeed (text s)
    in
        string |> andThen s2HtmlMsg

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
