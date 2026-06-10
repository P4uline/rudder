module DirectiveRecentActivity exposing (..)

import Activity.ApiCalls exposing (copy, errorNotification)
import Activity.DataTypes exposing (Activity, Msg(..))
import Activity.InitTooltips exposing (initTooltips)
import Activity.JsonDecoder exposing (decodeErrorDetails, decodeGetActivities)
import Activity.JsonEncoder exposing (encodeRestEventLogFilter)
import Browser
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http exposing (header, jsonBody, request)
import Http.Detailed as Detailed
import List.Nonempty as NonEmptyList
import Ordering exposing (Ordering)
import Rudder.Table exposing (..)
import Task
import Time exposing (Posix, Zone, millisToPosix, utc)
import TimeZone
import Url.Builder exposing (QueryParameter)
import Http exposing (Error)
import Http.Detailed




type alias DirectiveId =
    String



type alias Model =
    { directiveId : DirectiveId
    , activityTable : Rudder.Table.Model Activity Msg
    , contextPath : String
    , activities : List Activity
    , currentTime : Posix
    , zone : Zone
    }


type Msg
    = RudderTableMsg (Rudder.Table.Msg Msg)
    | CallApi (Model -> Cmd Msg)
    | GetActivities (Result (Http.Detailed.Error String) ( Http.Metadata, (List Activity) ))
    | Tick Posix
    | Copy String



getUrl: Model -> List String -> List QueryParameter -> String
getUrl m url p=
  Url.Builder.relative (m.contextPath :: "secure" :: "api" :: url) p


initTable : List Activity -> Rudder.Table.Model Activity Msg
initTable activities =
    let
        columns =
            NonEmptyList.Nonempty
                { name = ColumnName "Id", renderHtml = .id >> String.fromInt >> text, ordering = Ordering.byField .id }
                [ { name = ColumnName "Actor", renderHtml = .actor >> text, ordering = Ordering.byField .actor }
                , { name = ColumnName "Description", renderHtml = .description >> text, ordering = Ordering.byField .description }
                ]

        config =
            buildConfig.newConfig columns
                |> buildConfig.withOptions
                    (buildOptions.newOptions
                        |> buildOptions.withCustomizations
                            (buildCustomizations.newCustomizations
                                |> buildCustomizations.withTableContainerAttrs [ class "table-container" ]
                                |> buildCustomizations.withTableAttrs [ class "no-footer dataTable" ]
                            )
                    )



        data =
            [ { id = 1
              , actor = "Admin"
              , description = "Awesome directive 1"

              {- , date=Time.now -}
              }
            , { id = 2
              , actor = "Admin"
              , description = "Awesome directive 2"

              {- , date=Time.now -}
              }
            , { id = 3
              , actor = "Admin"
              , description = "Awesome directive 3"

              {- , date=Time.now -}
              }
            , { id = 4
              , actor = "Admin"
              , description = "Awesome directive 4"

              {- , date=Time.now -}
              }
            , { id = 5
              , actor = "Admin"
              , description = "Awesome directive 5"

              {- , date=Time.now -}
              }
            ]
    in
    -- Rudder.Table.init config data
    Rudder.Table.init config activities


{-init : { contextPath : String, timeZone: String } -> ( Model, Cmd Msg )
init flags =
    let
        initTimeZone =
            Dict.get flags.timeZone TimeZone.zones
                |> Maybe.withDefault (\() -> Time.utc)

        initModel =
            Model flags.contextPath [] (Time.millisToPosix 0) (initTimeZone ())

        initActions =
            [ getActivities initModel
            , initTooltips ""
            , Task.perform Tick Time.now
            ]
    in
    ( initModel, Cmd.batch initActions )-}

getActivities : Model -> Cmd Msg
getActivities model =
  let
    req =
      request
        { method  = "POST"
        , headers = [header "X-Requested-With" "XMLHttpRequest"]
        , url     = getUrl model [ "eventlog" ] []
        , body    = encodeRestEventLogFilter |> jsonBody
        , expect  = Detailed.expectJson GetActivities decodeGetActivities
        , timeout = Nothing
        , tracker = Nothing
        }
  in
    req

{- FIXME pass a timezone to the elm app, just as directiveId and contextPath and then use initTimeZone value instead of currentTime -}
init : { directiveId : String, contextPath : String{-, timeZone: String-} } -> ( Model, Cmd Msg )
init flags =
    let
        currentTime = millisToPosix 1000
        {-activityModel = Activity.DataTypes.Model
          flags.contextPath [] currentTime utc-}
        {-initTimeZone =
                    Dict.get flags.timeZone TimeZone.zones
                        |> Maybe.withDefault (\() -> Time.utc)-}
        initModel =
            Model flags.directiveId (initTable []) flags.contextPath [] currentTime utc
        initActions =
            [ getActivities initModel
            , initTooltips ""
            , Task.perform Tick Time.now {- FIXME why this ? do it -}
            ]

    in
    ( initModel, Cmd.batch initActions )




processApiError : String -> Detailed.Error String -> Model -> ( Model, Cmd Msg )
processApiError apiName err model =
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
    ( model, errorNotification ("Error when " ++ apiName ++ ", details: \n" ++ message) )


{- Table of the recent activity -}


table: Model -> Html Msg
table model =
    div [ class "main-table" ] [ Html.map RudderTableMsg (Rudder.Table.view model.activityTable) ]

view : Model -> Html Msg
view model =
    table model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RudderTableMsg m ->
            let
                ( activityTable, tableMsg, _ ) =
                    Rudder.Table.update m model.activityTable
            in
            ( { model | activityTable = activityTable }, tableMsg )
        CallApi call ->
          ( model, call model ) {- FIXME : why this ? remove this non expressive Msg, use only functional Message names -}
        GetActivities res ->
                    case res of
                        Ok ( _, activities ) ->
                            ( { model | activities = activities } {- FIXME i'm sure there is a better way to write this -}
                            , initTooltips ""
                            )

                        Err err ->
                            processApiError "Getting activities list" err model
        Tick newTime ->
             ( { model | currentTime = newTime }, Cmd.none )
        Copy s -> {- FIXME: Why is it useful ? -}
            ( model, copy s )


subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
