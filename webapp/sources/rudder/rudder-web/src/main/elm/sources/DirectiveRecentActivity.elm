module DirectiveRecentActivity exposing (..)

import Activity.ApiCalls exposing (copy, getActivities, processApiError)
import Activity.DataTypes exposing (Activity, ActivityMsg(..), ContextPath(..), Search(..))
import Activity.InitTooltips exposing (initTooltips)
import Browser
import DateFormat
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Nonempty as NonEmptyList
import Ordering exposing (Ordering)
import Rudder.Table exposing (..)
import Task
import Time exposing (Posix, Zone, millisToPosix, utc)
import Time.DateTime


type DirectiveId
    = DirectiveId String


type alias Model =
    { directiveId : DirectiveId
    , activityTable : Rudder.Table.Model Activity Msg
    , contextPath : ContextPath
    , currentTime : Posix
    , zone : Zone
    }


type Msg
    = CallApi (Model -> Cmd Msg)
    | RudderTableMsg (Rudder.Table.Msg Msg)
    | ActivityMessage ActivityMsg


initTable : Rudder.Table.Model Activity Msg
initTable =
    let
        columns =
            NonEmptyList.Nonempty
                { name = ColumnName "Id", renderHtml = .id >> String.fromInt >> text, ordering = Ordering.byField .id }
                [ { name = ColumnName "Actor", renderHtml = .actor >> text, ordering = Ordering.byField .actor }
                , { name = ColumnName "Description", renderHtml = .description >> text, ordering = Ordering.byField .description }
                -- FIXME
                {-, { name = ColumnName "Date", renderHtml =  >>  text, ordering = Ordering.byField .date }-}
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



{- FIXME pass a timezone to the elm app, just as directiveId and contextPath and then use initTimeZone value instead of currentTime -}


init :
    { directiveId : String
    , contextPath : String

    {- , timeZone: String -}
    }
    -> ( Model, Cmd Msg )
init flags =
    let
        currentTime =
            millisToPosix 1000

        initModel =
            Model (DirectiveId flags.directiveId) initTable (ContextPath flags.contextPath) currentTime utc

        -- Keep only directive activity filtering on event log types
        filterType =
            [ "AddDirective", "DeleteDirective", "ModifyDirective" ]

        -- full text search on directive id to keep activity related to this directive
        search =
            Search flags.directiveId

        initActions =
            [ Cmd.map ActivityMessage (getActivities search filterType initModel.contextPath)
            , initTooltips ()

            {- Call initTooltips javascript function to have beautiful customized fancy tooltips in elm app -}
            , Cmd.map ActivityMessage (Task.perform Tick Time.now)

            {- FIXME why this ? -}
            ]
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

        {- FIXME : why this ? remove this non expressive Msg, use only functional Message names -}
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
                            ( { model | activityTable = updatedTable }
                            , initTooltips () {- FIXME is this feature necessary for the recent activity ? do we want tooltips in the table -}
                            )

                        Err err ->
                            ( model, processApiError "Getting activities list" err )

                Tick newTime ->
                    ( { model | currentTime = newTime }, Cmd.none )

                Copy s ->
                    {- FIXME: Why is it useful ? -}
                    ( model, copy s )


subscriptions _ =
    Sub.batch
        [ Sub.map ActivityMessage (Time.every 1000 Tick) -- Update of the current time every second
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
