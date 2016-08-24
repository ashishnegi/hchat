module Hello exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Html.App
import Http
import Task exposing (Task)
import Json.Encode
import Json.Decode

-- Model
type alias Model =
    Bool

init : (Model, Cmd Msg)
init = (False, Cmd.none)

-- Messages
type Msg = Collapse
         | Expand

-- View
view : Model -> Html Msg
view model =
    if model then
        div []
            [button [onClick Collapse] [text "Collapse"]
            ,text "Widget"]
    else
        div []
            [button [onClick Expand] [text "Expand"]]

-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Collapse -> (False, Cmd.none)
        Expand   -> (True,  Cmd.none)

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
