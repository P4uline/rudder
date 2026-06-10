module Activity.DataTypes exposing (..)

import Http exposing (Error)
import Http.Detailed
import Time exposing (Posix, Zone)

--
-- All our data types
--

type alias ContextPath = String

type alias Activity =
    { id : Int
    , actor : String
    , description : String
    {-, date : Posix-} {- FIXME uncomment date -}
    }

type alias RestEventLogFilter =
    { draw: Int
    , start: Int
    , length: Int
    }

type alias EventLogFilterOrder =
    { column : Int
    , dir : String
    , name : String
    }

