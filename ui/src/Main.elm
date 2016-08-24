module Hello exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Html.App
import User.User as User

-- Model
type alias Model =
    { me : User.Model }

init : (Model, Cmd Msg)
init = ( Model (User.Model 1 "ashish" "")
       , Cmd.none)

-- Messages
type Msg = UserMsg User.Msg

-- View
view : Model -> Html Msg
view model =
    Html.App.map UserMsg (User.view model.me)

-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserMsg m -> let ( me', cmds ) = User.update m model.me
                     in ( { model | me = me' }
                        , Cmd.map UserMsg cmds )

-- Subscription
subscriptions : Model -> Sub Msg
subscriptions model = Sub.map UserMsg (User.subscriptions model.me)

main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
