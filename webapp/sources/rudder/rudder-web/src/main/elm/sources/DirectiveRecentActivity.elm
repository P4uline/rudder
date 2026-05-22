module DirectiveRecentActivity exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Nonempty as NonEmptyList
import Ordering exposing (Ordering)
import Rudder.Table exposing (..)

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
        columns =
            NonEmptyList.Nonempty
              { name = ColumnName "Id", renderHtml = .id >> String.fromInt >> text, ordering = Ordering.byField .id }
              [{ name = ColumnName "Actor", renderHtml = .actor >> text, ordering = Ordering.byField .actor }
              ,{ name = ColumnName "Description", renderHtml = .description >> text, ordering = Ordering.byField .description }]

        config = buildConfig.newConfig columns

        data = [
              { id=1, actor="Admin", description= "Awesome directive 1"{-, date=Time.now-} },
              { id=2, actor="Admin", description= "Awesome directive 2"{-, date=Time.now-} },
              { id=3, actor="Admin", description= "Awesome directive 3"{-, date=Time.now-} },
              { id=4, actor="Admin", description= "Awesome directive 4"{-, date=Time.now-} },
              { id=5, actor="Admin", description= "Awesome directive 5"{-, date=Time.now-} }
              ]
    in
      Rudder.Table.init config data


init : { directiveId : String, contextPath : String } -> ( Model, Cmd Msg )
init flags =
  let
    initModel = Model "123" flags.contextPath initTable {- TODO use type DirectiveId -}
  in
    (initModel, Cmd.none)


{- TODO remove this duplication, design a common place for activity (see Dashboard code to refactor) -}
type alias Activity =
    { id : Int
    , actor : String
    , description : String
    {-, date : Posix-}
    }

{- Table of the recent activity -}
table model =
  div [class "main-table"] [Html.map ShowActivityTable (Rudder.Table.view initTable)]

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