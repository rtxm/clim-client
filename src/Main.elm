module Main exposing (main)

--Third parties
-- Local imports

import Browser.Dom as Dom
import Html exposing (Html)
import Http
import Model exposing (..)
import Msg exposing (..)
import Task
import Time exposing (Time, second)
import View


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { probeData = [], mobile = True }
    , Cmd.batch [ getSamples, initialSize ]
    )


initialSize : Cmd Msg
initialSize =
    Window.size
        |> Task.perform NewWinSize



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( model, getSamples )

        NewSamples (Ok samples) ->
            ( { model | probeData = samples }, Cmd.none )

        NewSamples (Err _) ->
            ( model, Cmd.none )

        NewWinSize size ->
            ( { model | mobile = size.width < 850 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5 * second) Tick
        , Window.resizes NewWinSize
        ]



-- HTTP


getSamples : Cmd Msg
getSamples =
    let
        url =
            "http://alarmpi:8000"
    in
    Http.send NewSamples (Http.get url decodeSamples)
