module Main exposing (main)

import Browser
import Html exposing (Html, h1, text)



-- Model


type alias Model =
    {}


initialModel : Model
initialModel =
    {}



-- Msg


type Msg
    = NoOp



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- View


view : Model -> Html Msg
view model =
    h1 [] [ text "Hello Elm!" ]



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
