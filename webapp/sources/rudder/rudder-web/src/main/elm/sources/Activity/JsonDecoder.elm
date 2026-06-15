module Activity.JsonDecoder exposing (..)

import Activity.DataTypes exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (..)
import List exposing (drop, head)
import String exposing (join, split)
import Html.String as HtmlString


decodeGetActivities : Decoder (List (Activity msg))
decodeGetActivities =
    at [ "data" ] (list decodeActivity)


decodeActivity : Decoder (Activity msg)
decodeActivity =
    succeed Activity
        |> required "id" int
        |> required "actor" string
        |> required "description" stringToHtmlMsg
        |> required "date" Json.Decode.Extra.datetime


stringToHtmlMsg : Decoder (HtmlString.Html msg)
stringToHtmlMsg =
    let
        s2HtmlMsg : String -> Decoder (HtmlString.Html msg)
        s2HtmlMsg s = succeed (HtmlString.text s)
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
