port module Activity.ApiCalls exposing (..)

import Activity.DataTypes exposing (ContextPath)
import Activity.JsonDecoder exposing (decodeErrorDetails)
import Http.Detailed as Detailed
import Url.Builder exposing (QueryParameter)
port errorNotification : String -> Cmd msg
port copy : String -> Cmd msg

getUrl: ContextPath -> List String -> List QueryParameter -> String
getUrl contextPath url p=
  Url.Builder.relative (contextPath :: "secure" :: "api" :: url) p

processApiError : String -> Detailed.Error String -> Cmd msg
processApiError apiName err =
    let
        message =
            case err of
                Detailed.BadUrl url ->
                    "The URL " ++ url ++ " was invalid"

                Detailed.Timeout ->
                    "Unable to reach the server, try again"

                Detailed.NetworkError ->
                    "Unable to reach the server, check your network connection"

                Detailed.BadStatus metadata body ->
                    let
                        ( title, errors ) =
                            decodeErrorDetails body
                    in
                    title ++ "\n" ++ errors

                Detailed.BadBody metadata body msg ->
                    msg
    in
      errorNotification ("Error when " ++ apiName ++ ", details: \n" ++ message)

