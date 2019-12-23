module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- Model


type alias Model =
    { stack : List Float
    , currentNum : Float
    }


initialModel : Model
initialModel =
    { stack = []
    , currentNum = 0
    }



-- Msg


type Msg
    = InputNumber Float
    | Clear
    | ClearAll
    | Back
    | Enter
    | InputOperator Operator


type Operator
    = Add
    | Sub
    | Mult
    | Div



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputNumber num ->
            { model | currentNum = (model.currentNum * 10) + num }

        Clear ->
            { model | currentNum = 0 }

        ClearAll ->
            { model | currentNum = 0, stack = [] }

        Back ->
            { model | currentNum = toFloat <| floor <| model.currentNum / 10 }

        Enter ->
            { model
                | stack = model.currentNum :: model.stack
                , currentNum = 0
            }

        InputOperator operator ->
            case model.stack of
                [] ->
                    model

                x :: xs ->
                    let
                        op =
                            operatorFunction operator

                        newNum =
                            op x model.currentNum
                    in
                    { model
                        | stack = xs
                        , currentNum = newNum
                    }


operatorFunction : Operator -> (Float -> Float -> Float)
operatorFunction operator =
    case operator of
        Add ->
            (+)

        Sub ->
            (-)

        Mult ->
            (*)

        Div ->
            (/)



-- View


type Size
    = Single
    | Double
    | Trible


sizeToString : Size -> String
sizeToString size =
    case size of
        Single ->
            "single"

        Double ->
            "double"

        Trible ->
            "triple"


type Color
    = Yellow
    | Gray
    | White


colorToString : Color -> String
colorToString color =
    case color of
        Yellow ->
            "bg-yellow"

        Gray ->
            "bg-gray"

        White ->
            "bg-white"


cell : Html.Attribute Msg -> Size -> Color -> String -> Html Msg
cell attr size color content =
    Html.button
        [ class <|
            String.join " " <|
                [ "cell", sizeToString size, colorToString color ]
        , attr
        ]
        [ text content ]


inputBox : Float -> Html Msg
inputBox num =
    div [ class "input-box" ]
        [ text <| String.fromFloat num ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "h1" ] [ text "RPN Calculator" ]
        , div
            [ class "calculator" ]
            (List.map inputBox (List.reverse model.stack)
                ++ [ inputBox model.currentNum
                   , section
                   ]
            )
        ]


section : Html Msg
section =
    div
        [ class "section" ]
        [ cell (onClick Back) Single Gray "<-"
        , cell (onClick ClearAll) Single Gray "C"
        , cell (onClick Clear) Single Gray "CE"
        , cell (onClick (InputOperator Div)) Single Yellow "/"
        , cell (onClick (InputNumber 7)) Single White "7"
        , cell (onClick (InputNumber 8)) Single White "8"
        , cell (onClick (InputNumber 9)) Single White "9"
        , cell (onClick (InputOperator Mult)) Single Yellow "x"
        , cell (onClick (InputNumber 4)) Single White "4"
        , cell (onClick (InputNumber 5)) Single White "5"
        , cell (onClick (InputNumber 6)) Single White "6"
        , cell (onClick (InputOperator Sub)) Single Yellow "-"
        , cell (onClick (InputNumber 1)) Single White "1"
        , cell (onClick (InputNumber 2)) Single White "2"
        , cell (onClick (InputNumber 3)) Single White "3"
        , cell (onClick (InputOperator Add)) Single Yellow "+"
        , cell (onClick (InputNumber 0)) Single White "0"
        , cell (onClick (InputNumber 0)) Single White "."
        , cell (onClick Enter) Double Yellow "Enter"
        ]



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
