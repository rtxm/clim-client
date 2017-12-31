module Main exposing (..)

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Round
import String exposing (join)
import Svg
import Svg.Attributes as Svga
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Sample =
    ( Float, Date.Date )


type alias Model =
    List ( String, List Sample )


init : ( Model, Cmd Msg )
init =
    ( []
    , getSamples
    )



-- UPDATE


type Msg
    = Tick Time
    | NewSamples (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( model, getSamples )

        NewSamples (Ok samples) ->
            ( samples, Cmd.none )

        NewSamples (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map (\( key, samples ) -> card key samples) model)
        ]


formatTime : Date.Date -> String
formatTime date =
    let
        hour =
            toString (Date.hour date)

        minutes =
            toString (Date.minute date)
    in
        case String.length minutes of
            1 ->
                hour ++ ":0" ++ minutes

            _ ->
                hour ++ ":" ++ minutes


formatTemp : Float -> String
formatTemp t =
    Round.round 1 t


card : String -> List Sample -> Html Msg
card key samples =
    case List.head samples of
        Just ( temp, date ) ->
            div []
                [ h2 [] [ text (formatTemp temp), text "° ", text (formatTime date) ]
                , graph 360 220 samples
                ]

        Nothing ->
            div [] []


getRange : List Float -> ( Float, Float )
getRange values =
    let
        rawMin =
            Maybe.withDefault 10 (List.minimum values)

        rawMax =
            Maybe.withDefault 30 (List.maximum values)

        span =
            rawMax - rawMin
    in
        if span < 1 then
            let
                margin =
                    (1 - span) / 2
            in
                ( rawMin - margin, rawMax + margin )
        else
            ( rawMin, rawMax )


projX : Float -> Float -> Float -> Float -> String
projX xMargin xSpan dataSpan x =
    toString (xMargin + x * (xSpan / dataSpan))


projY : Float -> Float -> Float -> Float -> Float -> String
projY yMargin ySpan yMin yMax y =
    toString (ySpan + yMargin - (y - yMin) * ySpan / (yMax - yMin))


graph : Float -> Float -> List Sample -> Html Msg
graph gWidth gHeight samples =
    let
        ( temps, dates ) =
            List.unzip (List.reverse samples)

        ( yMin, yMax ) =
            getRange temps

        yStep =
            (yMax - yMin) / 10.0

        xMargin =
            30

        yMargin =
            20

        xSpan =
            gWidth - 2 * xMargin

        ySpan =
            gHeight - 2 * yMargin

        projY_ =
            projY yMargin ySpan yMin yMax

        projX_ =
            projX xMargin xSpan 19.0

        x0 =
            projX_ 0

        y0 =
            projY_ yMin

        points =
            temps
                |> List.indexedMap (\x y -> (projX_ <| toFloat x) ++ "," ++ (projY_ y))
                |> join " "
    in
        Svg.svg
            [ Svga.viewBox ("0 0 " ++ toString gWidth ++ " " ++ toString gHeight) ]
            --[ Svga.width <| toString gWidth, Svga.height <| toString gHeight ]
            [ Svg.g [ Svga.class "axis x-axis" ]
                [ Svg.line
                    [ Svga.x1 x0
                    , Svga.y1 y0
                    , Svga.x2 (projX_ 19.0)
                    , Svga.y2 y0
                    , Svga.stroke "#ccc"
                    ]
                    []
                ]
            , Svg.g [ Svga.class "labels x-labels" ]
                (dates
                    |> List.indexedMap
                        (\x d ->
                            case x % 2 of
                                1 ->
                                    text ""

                                _ ->
                                    Svg.text_
                                        [ Svga.x (projX_ <| toFloat x)
                                        , Svga.y y0
                                        , Svga.dy <| "13"
                                        , Svga.dx <| toString (-xMargin / 2)

                                        --, Svga.style "writing-mode: tb"
                                        , Svga.fontSize "10"
                                        ]
                                        [ Svg.text <| formatTime d ]
                        )
                )
            , Svg.g [ Svga.class "axis y-axis" ]
                [ Svg.line
                    [ Svga.x1 x0
                    , Svga.y1 y0
                    , Svga.x2 x0
                    , Svga.y2 (projY_ yMax)
                    , Svga.stroke "#ccc"
                    ]
                    []
                ]
            , Svg.g [ Svga.class "labels y-labels" ]
                ((List.range 0 10)
                    |> List.map
                        (\y ->
                            let
                                t =
                                    (toFloat y) * yStep + yMin
                            in
                                Svg.text_
                                    [ Svga.x x0
                                    , Svga.y (projY_ t)
                                    , Svga.fontSize "10"
                                    , Svga.dx "-23"
                                    , Svga.dy "3"
                                    ]
                                    [ Svg.text <| formatTemp t ]
                        )
                )
            , Svg.polyline [ Svga.fill "none", Svga.stroke "green", Svga.points points ] []
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (5 * second) Tick



-- HTTP


getSamples : Cmd Msg
getSamples =
    let
        url =
            "http://alarmpi:8000"
    in
        Http.send NewSamples (Http.get url decodeSamples)


makeSample : Float -> Int -> Sample
makeSample t s =
    ( t, Date.fromTime (second * toFloat s) )


sampleDecoder : Decode.Decoder Sample
sampleDecoder =
    Decode.map2 makeSample (Decode.index 0 Decode.float) (Decode.index 1 Decode.int)


decodeSamples : Decode.Decoder Model
decodeSamples =
    Decode.keyValuePairs (Decode.list sampleDecoder)
