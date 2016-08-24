module User.User exposing (..)

import Html exposing (Html, text, div, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type', value, class)

type alias UserId = Int
type alias UserName = String
type alias UserMessage = String

type alias Model =
    { id : UserId
    , name : UserName
    , bufText : UserMessage
    }

type Msg = SendMsg
         | Input String

view : Model -> Html Msg
view model =
    div [ class "inputfield" ]
        [ div [ class "name username text" ] [text model.name]
        , input [onInput Input, value model.bufText, class "input send" ] []
        , button [onClick SendMsg, class "button send" ] [text "Send"]]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendMsg -> (model, Cmd.none)
        Input bText -> ( { model | bufText = bText }
                       , Cmd.none )
