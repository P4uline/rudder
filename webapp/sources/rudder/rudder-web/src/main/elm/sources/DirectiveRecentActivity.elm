module DirectiveRecentActivity exposing (..)

import Browser
import Html exposing (text)

type alias DirectiveId = String

type alias Model =
  { directiveId : DirectiveId
  , contextPath : String
  }

type alias Msg = String -- ex: CallChangeLogsApi


init : { directiveId : String, contextPath : String } -> ( Model, Cmd Msg )
init flags = (flags, Cmd.none)

view model = text "Hello recent activity world"

update message model = (model, Cmd.none)

subscriptions model = Sub.none

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }