module DirectiveRecentActivity exposing (..)

import Activity.ApiCalls exposing (copy, getActivities, processApiError)
import Activity.DataTypes exposing (Activity, ActivityMsg(..), ContextPath(..), Search(..))
import Browser
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Nonempty as NonEmptyList exposing (Nonempty)
import Ordering exposing (Ordering)
import Rudder.Table exposing (..)
import Time exposing (Posix, Zone)
import TimeZone
import Utils.DateUtils exposing (posixToString)
import Html.String


type DirectiveId
    = DirectiveId String


type alias Model =
    { directiveId : DirectiveId
    , activityTable : Rudder.Table.Model (Activity Msg) Msg
    , contextPath : ContextPath
    , zone : Zone
    }


type Msg
    = CallApi (Model -> Cmd Msg)
    | RudderTableMsg (Rudder.Table.Msg Msg)
    | ActivityMessage (ActivityMsg Msg)


initTable : Zone -> Rudder.Table.Model (Activity Msg) Msg
initTable timezone =
    let

        orderingDesc : (Activity Msg) -> String
        orderingDesc act= act.description |> Html.String.toString 0
        columns :  Nonempty (Column (Activity Msg) Msg)
        columns =  NonEmptyList.Nonempty
                { name = ColumnName "Id", renderHtml = .id >> String.fromInt >> text, ordering = Ordering.byField .id }
                [ { name = ColumnName "Actor", renderHtml = .actor >> text, ordering = Ordering.byField .actor }
                , { name = ColumnName "Description", renderHtml = (.description >> Html.String.toHtml) , ordering = Ordering.byField (.description >> Html.String.toString 0) }
                , { name = ColumnName "Date", renderHtml = .date >> posixToString timezone >> text, ordering = Ordering.byField (.date >> Time.posixToMillis) }
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
    in
    Rudder.Table.init config []


init :
    { directiveId : String
    , contextPath : String
    , timeZone : String
    }
    -> ( Model, Cmd Msg )
init flags =
    let
        initTimeZone =
            Dict.get flags.timeZone TimeZone.zones
                |> Maybe.withDefault (\() -> Time.utc)

        zone =
            initTimeZone ()

        initModel =
            Model (DirectiveId flags.directiveId) (initTable zone) (ContextPath flags.contextPath) zone

        -- Keep only directive activity filtering on event log types
        filterType =
            [ "AddDirective", "DeleteDirective", "ModifyDirective" ]

        -- Full text search on directive id to keep activity related to this directive
        search =
            Search flags.directiveId

        initActions =
            [ Cmd.map ActivityMessage (getActivities search filterType initModel.contextPath) ]
    in
    ( initModel, Cmd.batch initActions )



{- Table of the recent activity -}


table : Model -> Html Msg
table model =
    div [ class "main-table" ] [ Html.map RudderTableMsg (Rudder.Table.view model.activityTable) ]


view : Model -> Html Msg
view model =
    table model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CallApi call ->
            ( model, call model )

        RudderTableMsg m ->
            let
                ( activityTable, tableMsg, _ ) =
                    Rudder.Table.update m model.activityTable
            in
            ( { model | activityTable = activityTable }, tableMsg )

        ActivityMessage a ->
            case a of
                GetActivities res ->
                    case res of
                        -- Update table data
                        Ok ( _, activities ) ->
                            let
                                updatedTable =
                                    updateData activities model.activityTable
                            in
                            ( { model | activityTable = updatedTable }, Cmd.none )

                        Err err ->
                            ( model, processApiError "Getting activities list" err )

                CopyToClipboard s ->
                    ( model, copy s )


subscriptions _ =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
