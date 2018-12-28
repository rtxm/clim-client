module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Round
import String exposing (join)
import Svg
import Svg.Attributes as Svga
import Task
import Time


type alias Sample =
    ( Float, Time.Posix )


type alias ProbeSamples =
    List ( String, List Sample )


type alias Model =
    { probeData : ProbeSamples
    , mobile : Bool
    , zone : Time.Zone
    }


makeSample : Float -> Int -> Sample
makeSample t s =
    ( t, Time.millisToPosix (1000 * s) )


sampleDecoder : Decode.Decoder Sample
sampleDecoder =
    Decode.map2 makeSample (Decode.index 0 Decode.float) (Decode.index 1 Decode.int)


decodeSamples : Decode.Decoder ProbeSamples
decodeSamples =
    Decode.keyValuePairs (Decode.list sampleDecoder)


type Msg
    = Tick Time.Posix
    | NewSamples (Result Http.Error ProbeSamples)
    | NewWinSize Int Int
    | AdjustTimeZone Time.Zone


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : Int -> ( Model, Cmd Msg )
init width =
    ( { probeData = [], mobile = width < 850, zone = Time.utc }
    , Cmd.batch [ getSamples, Task.perform AdjustTimeZone Time.here ]
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

        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 8000 Tick
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



-- VIEW


view : Model -> Document Msg
view model =
    let
        container =
            if model.mobile then
                class "App"

            else
                class "App"
    in
    { title = "Temperadur er gêr"
    , body =
        [ div [ container ]
            (List.map (\( key, samples ) -> card key samples model.zone) model.probeData)
        ]
    }


formatTime : Time.Posix -> Time.Zone -> String
formatTime date zone =
    let
        hour =
            String.fromInt (Time.toHour zone date)

        minutes =
            String.fromInt (Time.toMinute zone date)
    in
    case String.length minutes of
        1 ->
            hour ++ ":0" ++ minutes

        _ ->
            hour ++ ":" ++ minutes


formatDate : Time.Posix -> Time.Zone -> String
formatDate date zone =
    let
        day =
            Time.toDay zone date |> String.fromInt

        year =
            Time.toYear zone date |> String.fromInt

        month =
            case Time.toMonth zone date of
                Time.Jan ->
                    "01"

                Time.Feb ->
                    "02"

                Time.Mar ->
                    "03"

                Time.Apr ->
                    "04"

                Time.May ->
                    "05"

                Time.Jun ->
                    "06"

                Time.Jul ->
                    "07"

                Time.Aug ->
                    "08"

                Time.Sep ->
                    "09"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"
    in
    day ++ "/" ++ month ++ "/" ++ year


formatTemp : Float -> String
formatTemp t =
    Round.round 1 t


locate : String -> String
locate key =
    case key of
        "0" ->
            "Salon"

        "1" ->
            "Bureau"

        "2" ->
            "Mobile"

        _ ->
            "Inconnu"


card : String -> List Sample -> Time.Zone -> Html Msg
card key samples zone =
    case List.head samples of
        Just ( temp, date ) ->
            div [ class "Card" ]
                [ h2 [] [ text <| locate key ]
                , div [ class "subtitle" ]
                    [ text <| formatDate date zone
                    , text " "
                    , text <| formatTime date zone
                    ]
                , div [ class "temperature" ] [ text <| formatTemp temp ++ "°" ]
                , graph 360 230 samples zone
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
    String.fromFloat (xMargin + x * (xSpan / dataSpan))


projY : Float -> Float -> Float -> Float -> Float -> String
projY yMargin ySpan yMin yMax y =
    String.fromFloat (ySpan + yMargin - (y - yMin) * ySpan / (yMax - yMin))


graph : Float -> Float -> List Sample -> Time.Zone -> Svg.Svg msg
graph gWidth gHeight samples zone =
    let
        ( temps, dates ) =
            List.unzip (List.reverse samples)

        ( yMin, yMax ) =
            getRange temps

        yStep =
            (yMax - yMin) / 10.0

        xMargin =
            0

        yMargin =
            25

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
                |> List.indexedMap (\x y -> (projX_ <| toFloat x) ++ "," ++ projY_ y)
                |> join " "
    in
    Svg.svg
        [ Svga.viewBox ("0 0 " ++ String.fromFloat gWidth ++ " " ++ String.fromFloat gHeight)
        , Svga.class "Graph"
        ]
        --[ Svga.width <| String.fromFloat gWidth, Svga.height <| String.fromFloat gHeight ]
        [ let
            llCorner =
                "0," ++ String.fromFloat gHeight ++ " "

            lrCorner =
                " " ++ String.fromFloat gWidth ++ "," ++ String.fromFloat gHeight
          in
          Svg.polygon [ Svga.fill "#86b8ef", Svga.stroke "none", Svga.points (llCorner ++ points ++ lrCorner) ] []
        , Svg.g [ Svga.class "labels x-labels" ]
            (dates
                |> List.indexedMap
                    (\x d ->
                        case modBy 2 x of
                            1 ->
                                Svg.text ""

                            _ ->
                                Svg.text_
                                    [ Svga.x (projX_ <| toFloat x)
                                    , Svga.y y0
                                    , Svga.dy <| "20"
                                    , Svga.dx <| String.fromFloat -10

                                    --, Svga.style "writing-mode: tb"
                                    , Svga.fontSize "10"
                                    ]
                                    [ Svg.text <| formatTime d zone ]
                    )
            )
        , Svg.g [ Svga.class "labels y-labels" ]
            (List.range 0 10
                |> List.map
                    (\y ->
                        let
                            t =
                                toFloat y * yStep + yMin
                        in
                        Svg.text_
                            [ Svga.x x0
                            , Svga.y (projY_ t)
                            , Svga.fontSize "12"
                            , Svga.dx "5"
                            , Svga.dy "3"
                            , Svga.fill "white"
                            ]
                            [ Svg.text <| formatTemp t ]
                    )
            )
        ]
