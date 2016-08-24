module User.User exposing (..)

import Html exposing (Html, text, div, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type', value)

type alias UserId = Int

type alias Model =
    { id : UserId
    , name : String
    , bufText : String
    }

type Msg = SendMsg
         | Input String

view : Model -> Html Msg
view model =
    div []
        [ text model.name
        , input [onInput Input, value model.bufText] []
        , button [onClick SendMsg] [text "Send"]]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendMsg -> (model, Cmd.none)
        Input bText -> ( { model | bufText = bText }
                       , Cmd.none )
