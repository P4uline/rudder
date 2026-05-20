module DirectiveRecentActivity exposing (..)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (class)
import List.Nonempty as NonEmptyList
import Rudder.Table exposing (Column, ColumnName(..))
import Time exposing (Posix, Zone)
import Time.Date

type alias DirectiveId = String

type alias Model =
  { directiveId : DirectiveId
  , contextPath : String
  , activityTable : Rudder.Table.Model Activity Msg
  }

type Msg = ShowActivityTable (Rudder.Table.Msg Msg)

initTable : Rudder.Table.Model Activity Msg
initTable =
    let
        customizations =
            buildCustomizations.newCustomizations
                |> buildCustomizations.withTableContainerAttrs [class "table-container"]
                |> buildCustomizations.withTableAttrs [class "no-footer dataTable"]
                |> buildCustomizations.withTrAttrs (\rule -> [onClick (OpenRuleDetails rule.id True)])

        options =
            buildOptions.newOptions
                |> buildOptions.withCustomizations customizations
                |> buildOptions.withCsvExport
                    { entryToStringList = entryToStringList, btnAttributes=[class "d-none"]}

        columns : NonEmptyList.Nonempty (Column Activity Msg)
        columns =
            (NonEmptyList.Nonempty
                { name = (ColumnName "Id")
                , renderHtml = (\rule ->
                    div [] [ badgePolicyModeNoGlobal rule.policyMode
                           , text rule.name
                           , buildTagsTree rule.tags] )
                , ordering = Ordering.byField (.name >> String.toLower) }
                [ { name = (ColumnName "Category")
                  , renderHtml = .categoryName >> text
                  , ordering = Ordering.byField (.categoryName >> String.toLower) }
                , { name = (ColumnName "Status")
                  , renderHtml = .status >> (\s ->
                      let status = text s.value in
                      case s.details of
                        Just ms ->
                         span
                           [ class "disabled"
                           , attribute "data-bs-toggle" "tooltip"
                           , attribute "data-bs-placement" "top"
                           , title (buildTooltipContent "Reason(s)" ms)]
                           [ status, i[class "fa fa-info-circle"][]]
                        Nothing -> span[][ status ]
                      )
                  , ordering = Ordering.byField (.status >> .value >> String.toLower) }
                , { name = (ColumnName "Compliance")
                  , renderHtml = (\rule ->
                      case rule.compliance of
                        Just co ->
                          buildComplianceBar defaultComplianceFilter co.complianceDetails
                        Nothing -> div[class "skeleton-loading"][span[][]]
                    )
                  , ordering = Ordering.byField (.compliance >> complianceToString >> String.toLower) }
                , { name = (ColumnName "Changes")
                  , renderHtml = .changes >> String.fromFloat >> text
                  , ordering = Ordering.byField (.changes) }])

        config = buildConfig.newConfig columns |> buildConfig.withOptions options
    in
    Rudder.Table.init config []


init : { directiveId : String, contextPath : String, activityTable : Rudder.Table.Model Activity Msg} -> ( Model, Cmd Msg )
init flags = (flags, Cmd.none)

{- TODO remove this duplication, design a common place for activity (see Dashboard code to refactor) -}
type alias Activity =
    { id : Int
    , actor : String
    , description : String
    , date : Posix
    }

{- Table of the recent activity -}
table model =
  let

    activityList = [
      { id=1, actor="Admin", descrption= "Awesome directive 1", date=Time.now },
      { id=2, actor="Admin", descrption= "Awesome directive 2", date=Time.now },
      { id=3, actor="Admin", descrption= "Awesome directive 3", date=Time.now },
      { id=4, actor="Admin", descrption= "Awesome directive 4", date=Time.now },
      { id=5, actor="Admin", descrption= "Awesome directive 5", date=Time.now }
      ]
  in
    div [class "main-table"] [Html.map ShowActivityTable (Rudder.Table.view model.rulesTable)]

view model = table model

update message model = (model, Cmd.none)

subscriptions model = Sub.none

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }