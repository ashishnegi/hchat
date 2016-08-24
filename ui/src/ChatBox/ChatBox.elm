module ChatBox.ChatBox exposing (..)

import User.User exposing (UserId, UserName, UserMessage)
import Html exposing (Html, text, div)
import Html.Attributes exposing (class)

type alias MessageData = (UserId, UserName, UserMessage)
type alias Model =
    { msgs : List MessageData }

type Msg = NewMsg MessageData

view : Model -> Html Msg
view model =
    div [ class "chatbox" ]
        -- reverse render the messages : latest at bottom.
        ( List.map msgView (List.reverse model.msgs) )

msgView : MessageData -> Html Msg
msgView (id, name, msg) =
    div [ class "message" ]
        [ div [ class "name username text" ] [text (name ++ " : ")]
        , div [ class "usermsg text" ] [text msg]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewMsg data ->
            ( { model | msgs = data :: model.msgs }
            , Cmd.none )
