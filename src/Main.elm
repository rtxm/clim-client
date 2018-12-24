module Main exposing (main)

import Browser
import Browser.Events
import Http
import Task
import Time

import Model exposing (..)
import Msg exposing (..)
import View


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : Int -> ( Model, Cmd Msg )
init width =
    ( { probeData = [], mobile = width < 800 }
    , getSamples
    )


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

        NewWinSize width _ ->
            ( { model | mobile = width < 850 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (8000) Tick
        , Browser.Events.onResize NewWinSize
        ]



-- HTTP


getSamples : Cmd Msg
getSamples =
    let
        url =
            "http://alarmpi:8000"
    in
    Http.send NewSamples (Http.get url decodeSamples)
