import String

import Html.App
import Html exposing (div, text)

import SqlBuilder exposing (..)

--------------------------------------------------------------------------------

-- boilerplate to show the generated SQL query in a browser

init = ()

type alias Model = ()

update : Msg -> Model -> Model
update msg model =
  model

type Msg = NoOp

main =
  Html.App.beginnerProgram
    { model = init
    , update = update
    , view = view
    }



view model =
  div [] [ text sql ]
