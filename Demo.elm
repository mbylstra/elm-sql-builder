import String

import Html.App
import Html exposing (div, text, pre)

import Example

--------------------------------------------------------------------------------

-- boilerplate to show some basic html

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
  pre [] [ text Example.sql ]
