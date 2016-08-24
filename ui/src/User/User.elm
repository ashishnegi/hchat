module User.User exposing (..)

import Html exposing (Html, text, div, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type', value)
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=))
import WebSocket

type alias UserId = Int

type alias Model =
    { id : UserId
    , name : String
    , bufText : String
    }

type Msg = SendMsg
         | Input String
         | NewMessage Int String String
         | NoOp

view : Model -> Html Msg
view model =
    div []
        [ text model.name
        , input [onInput Input, value model.bufText] []
        , button [onClick SendMsg] [text "Send"]]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendMsg -> (model, sendMsg model)
        Input bText -> ( { model | bufText = bText }
                       , Cmd.none )
        NewMessage id name msg -> (model, Cmd.none)
        NoOp -> (model, Cmd.none)

wsserver : String
wsserver = "ws://echo.websocket.org"

sendMsg : Model -> Cmd Msg
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

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsserver decodeModel

decodeModel : String -> Msg
decodeModel json =
    let decoder = Decode.object3 NewMessage
                  ("id" := Decode.int)
                  ("name" := Decode.string)
                  ("msg" := Decode.string)
        res     = Decode.decodeString decoder json
    in case res of
           Ok m -> m
           Err s -> NoOp
