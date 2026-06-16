module Activity.DataTypes exposing (..)

import Html exposing (Html)
import Http exposing (Error)
import Http.Detailed
import Time exposing (Posix, Zone)



--
-- All our data types
--


type ContextPath
    = ContextPath String


type Search
    = Search String


type FilterType
    = List String


type alias Activity msg =
    { id : Int
    , actor : String
    , description : Html msg
    , date : Posix
    }


type alias RestEventLogFilter =
    { draw : Int
    , start : Int
    , length : Int
    }


type alias EventLogFilterOrder =
    { column : Int
    , dir : String
    , name : String
    }


type ActivityMsg
    = GetActivities (Result (Http.Detailed.Error String) ( Http.Metadata, List (Activity ActivityMsg)))
    | CopyToClipboard String
