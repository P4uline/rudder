port module Activity.ApiCalls exposing (..)

import Activity.DataTypes exposing (ActivityMsg(..), ContextPath(..), Search)
import Activity.JsonDecoder exposing (decodeErrorDetails, decodeGetActivities)
import Activity.JsonEncoder exposing (encodeRestEventLogFilter)
import Http exposing (header, jsonBody, request)
import Http.Detailed as Detailed
import Url.Builder exposing (QueryParameter)
port errorNotification : String -> Cmd msg
port copy : String -> Cmd msg

getUrl: ContextPath -> List String -> List QueryParameter -> String
getUrl (ContextPath contextPath) url p=
  Url.Builder.relative (contextPath :: "secure" :: "api" :: url) p

getActivities : Search -> List String -> ContextPath ->  Cmd ActivityMsg
getActivities search filterType contextPath =
  let
    req =
      request
        { method  = "POST"
        , headers = [header "X-Requested-With" "XMLHttpRequest"]
        , url     = getUrl contextPath [ "eventlog" ] []
        , body    = encodeRestEventLogFilter search filterType |> jsonBody
        , expect  = Detailed.expectJson GetActivities decodeGetActivities
        , timeout = Nothing
        , tracker = Nothing
        }
  in
    req


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

