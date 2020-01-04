module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as KK



-- Model


type alias Model =
    { stack : List Float
    , currentNum : String
    , error : Maybe String
    , dirty : Bool
    }


initialModel : Model
initialModel =
    { stack = []
    , currentNum = "0"
    , error = Nothing
    , dirty = False
    }



-- Msg


type Msg
    = InputNumber Float
    | Clear
    | ClearAll
    | Back
    | Enter
    | InputOperator Operator
    | SetDecimal
    | SetSign
    | HandleKeyboardEvent KeyboardEvent


type Operator
    = Add
    | Sub
    | Mult
    | Div



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputNumber num ->
            if model.currentNum == "0" then
                ( { model | currentNum = String.fromFloat num }
                , Cmd.none
                )

            else if model.dirty then
                ( { model
                    | currentNum = String.fromFloat num
                    , dirty = False
                  }
                , Cmd.none
                )

            else
                ( { model | currentNum = model.currentNum ++ String.fromFloat num }
                , Cmd.none
                )

        Clear ->
            ( { model | currentNum = "0", dirty = False }
            , Cmd.none
            )

        ClearAll ->
            ( initialModel
            , Cmd.none
            )

        Back ->
            let
                newNum =
                    String.dropRight 1 model.currentNum
            in
            ( { model
                | currentNum =
                    if String.isEmpty newNum then
                        "0"

                    else
                        newNum
              }
            , Cmd.none
            )

        Enter ->
            let
                maybeNumber =
                    String.toFloat model.currentNum
            in
            case maybeNumber of
                Nothing ->
                    ( { model | error = Just "Parse Error!" }
                    , Cmd.none
                    )

                Just num ->
                    ( { model
                        | stack = num :: model.stack
                        , dirty = True
                      }
                    , Cmd.none
                    )

        InputOperator operator ->
            case model.stack of
                [] ->
                    ( model, Cmd.none )

                x :: xs ->
                    let
                        op =
                            operatorFunction operator

                        maybeNumber =
                            String.toFloat model.currentNum
                    in
                    case maybeNumber of
                        Nothing ->
                            ( { model | error = Just "Parse Error!" }
                            , Cmd.none
                            )

                        Just num ->
                            let
                                newNum =
                                    op x num
                            in
                            ( { model
                                | stack = xs
                                , currentNum = String.fromFloat newNum
                                , dirty = False
                              }
                            , Cmd.none
                            )

        SetDecimal ->
            if String.contains "." model.currentNum then
                ( model, Cmd.none )

            else
                ( { model | currentNum = model.currentNum ++ "." }
                , Cmd.none
                )

        SetSign ->
            -- don't allow negative zero
            if model.currentNum == "0" then
                ( model, Cmd.none )
                -- from negative to positive

            else if String.startsWith "-" model.currentNum then
                ( { model
                    | currentNum =
                        String.dropLeft 1 model.currentNum
                  }
                , Cmd.none
                )
                -- from positive to negative

            else
                ( { model | currentNum = "-" ++ model.currentNum }
                , Cmd.none
                )

        HandleKeyboardEvent event ->
            case event.keyCode of
                KK.Add ->
                    update (InputOperator Add) model

                KK.Subtract ->
                    update (InputOperator Sub) model

                KK.Multiply ->
                    update (InputOperator Mult) model

                KK.Divide ->
                    update (InputOperator Div) model

                KK.Decimal ->
                    update SetDecimal model

                KK.Enter ->
                    update Enter model

                KK.Backspace ->
                    update Back model

                KK.NumpadZero ->
                    update (InputNumber 0) model

                KK.NumpadNine ->
                    update (InputNumber 9) model

                KK.NumpadOne ->
                    update (InputNumber 1) model

                KK.NumpadTwo ->
                    update (InputNumber 2) model

                KK.NumpadThree ->
                    update (InputNumber 3) model

                KK.NumpadFour ->
                    update (InputNumber 4) model

                KK.NumpadFive ->
                    update (InputNumber 5) model

                KK.NumpadSix ->
                    update (InputNumber 6) model

                KK.NumpadSeven ->
                    update (InputNumber 7) model

                KK.NumpadEight ->
                    update (InputNumber 8) model

                _ ->
                    ( model, Cmd.none )


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


stackBox : Float -> Html Msg
stackBox num =
    div
        [ class "input-box"
        ]
        [ text <| String.fromFloat num
        ]


inputBox : Html Msg -> Html Msg
inputBox num =
    div [ class "input-box" ]
        [ num ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "h1" ] [ text "RPN Calculator" ]
        , div
            [ class "calculator" ]
            (List.map stackBox (List.reverse model.stack)
                ++ [ inputBox <|
                        case model.error of
                            Nothing ->
                                text model.currentNum

                            Just err ->
                                span [ class "error" ] [ text err ]
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
        , cell (onClick SetDecimal) Single White "."
        , cell (onClick SetSign) Single White "+/-"
        , cell (onClick Enter) Single Yellow "Enter"
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (Decode.map HandleKeyboardEvent decodeKeyboardEvent)
