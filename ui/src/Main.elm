module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Html.App
import WebSocket
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode

import User.User as User
import ChatBox.ChatBox as ChatBox

main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { me : User.Model
    , chatBox : ChatBox.Model }

init : (Model, Cmd Msg)
init = ( Model (User.Model 1 "ashish" "")
               (ChatBox.Model [])
       , Cmd.none)

type Msg = UserMsg User.Msg
         | ChatBoxMsg ChatBox.Msg
         | NewMessage Int String String
         | NoOp

view : Model -> Html Msg
view model =
    div []
        [ Html.App.map ChatBoxMsg (ChatBox.view model.chatBox)
        , Html.App.map UserMsg (User.view model.me) ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserMsg m ->
            let ( me', cmds ) = User.update m model.me
                extraCommand = case m of
                                   User.SendMsg -> sendMsg me'
                                   _            -> Cmd.none

            in ( { model | me = me' }
               , Cmd.batch [ Cmd.map UserMsg cmds
                           , extraCommand ] )

        NewMessage id name message -> let (chatBox', cmds) = ChatBox.update (ChatBox.NewMsg (id, name, message)) model.chatBox
                                      in ( { model | chatBox = chatBox' }
                                      , Cmd.map ChatBoxMsg cmds )
        _ -> ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    let decodeMsg json =
            let decoder = Decode.object3 NewMessage
                            ("id" := Decode.int)
                            ("name" := Decode.string)
                            ("msg" := Decode.string)
                res     = Decode.decodeString decoder json
            in case res of
                   Ok m -> m
                   Err s -> NoOp
    in WebSocket.listen wsserver decodeMsg

sendMsg : User.Model -> Cmd Msg
sendMsg model =
    let body = memberEncode model
               |> Encode.encode 0
        memberEncode model
            = [ ("id", Encode.int model.id)
              , ("name", Encode.string model.name)
              , ("msg", Encode.string model.bufText)
              ]
              |> Encode.object
    in WebSocket.send wsserver body

wsserver : String
wsserver = "ws://echo.websocket.org"
