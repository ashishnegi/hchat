module Hello exposing (..)

import Html exposing (Html, text, div)
import Html.App

-- Model
type alias Model =
    String

init : (Model, Cmd Msg)
init = ("Hello", Cmd.none)

-- Messages
type Msg =
    NoOp

-- View
view : Model -> Html Msg
view model =
    div []
        [text model]

-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)

-- Subscription
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
