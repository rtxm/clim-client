module Main exposing (..)

import Color
import Date
import Debug
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Round
import String exposing (join)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Svg
import Svg.Attributes as Svga
import Task
import Time exposing (Time, second)
import Window


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


type alias ProbeSamples =
    List ( String, List Sample )


type alias Model =
    { probeData : ProbeSamples
    , device : Element.Device
    }


init : ( Model, Cmd Msg )
init =
    ( { probeData = [], device = Element.classifyDevice { width = 0, height = 0 } }
    , Cmd.batch [ getSamples, initialSize ]
    )


initialSize : Cmd Msg
initialSize =
    Window.size
        |> Task.perform NewWinSize



-- UPDATE


type Msg
    = Tick Time
    | NewSamples (Result Http.Error ProbeSamples)
    | NewWinSize Window.Size


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
            ( { model | device = Element.classifyDevice (Debug.log "size" size) }, Cmd.none )



-- VIEW


type MyStyles
    = None
    | Card
    | Heading
    | SubTitle
    | Temperature


sansSerif : List Font
sansSerif =
    [ Font.font "helvetica"
    , Font.font "arial"
    , Font.font "sans-serif"
    ]


stylesheet : StyleSheet MyStyles variation
stylesheet =
    Style.styleSheet
        [ style None [] -- It's handy to have a blank style
        , style Card
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.black
            , Color.background Color.lightBlue
            , Color.border Color.lightGrey
            , Font.typeface sansSerif
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , style Heading
            [ Font.typeface sansSerif
            , Font.size 32
            ]
        , style SubTitle
            [ Font.size 24
            , Color.text Color.darkCharcoal
            ]
        , style Temperature
            [ Font.size 96
            , Color.text Color.white
            ]
        ]


view : Model -> Html Msg
view model =
    let
        container =
            (if model.device.width > 850 then
                row
             else
                column
            )
    in
        Element.layout stylesheet <|
            container None
                [ spacing 20, padding 20 ]
                (List.map (\( key, samples ) -> card key samples) model.probeData)


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


locate : String -> String
locate key =
    case key of
        "0" ->
            "Bureau"

        "1" ->
            "Salon"

        "2" ->
            "Exterieur"

        _ ->
            "Inconnu"


card : String -> List Sample -> Element MyStyles variation Msg
card key samples =
    case List.head samples of
        Just ( temp, date ) ->
            column Card
                [ width (px 400) ]
                [ h2 Heading [ paddingLeft 10 ] (text <| locate key)
                , subheading SubTitle [ paddingLeft 10 ] (formatTime date)
                , el Temperature [ center ] (text <| (formatTemp temp) ++ "°")
                , html (graph 360 230 samples)
                ]

        Nothing ->
            empty


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


graph : Float -> Float -> List Sample -> Svg.Svg Msg
graph gWidth gHeight samples =
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
                |> List.indexedMap (\x y -> (projX_ <| toFloat x) ++ "," ++ (projY_ y))
                |> join " "
    in
        Svg.svg
            [ Svga.viewBox ("0 0 " ++ toString gWidth ++ " " ++ toString gHeight) ]
            --[ Svga.width <| toString gWidth, Svga.height <| toString gHeight ]
            [ let
                llCorner =
                    "0," ++ (toString gHeight) ++ " "

                lrCorner =
                    " " ++ (toString gWidth) ++ "," ++ (toString gHeight)
              in
                Svg.polygon [ Svga.fill "#86b8ef", Svga.stroke "none", Svga.points (llCorner ++ points ++ lrCorner) ] []
            , Svg.g [ Svga.class "labels x-labels" ]
                (dates
                    |> List.indexedMap
                        (\x d ->
                            case x % 2 of
                                1 ->
                                    Svg.text ""

                                _ ->
                                    Svg.text_
                                        [ Svga.x (projX_ <| toFloat x)
                                        , Svga.y y0
                                        , Svga.dy <| "20"
                                        , Svga.dx <| toString (-10)

                                        --, Svga.style "writing-mode: tb"
                                        , Svga.fontSize "10"
                                        ]
                                        [ Svg.text <| formatTime d ]
                        )
                )
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
                                    , Svga.fontSize "12"
                                    , Svga.dx "5"
                                    , Svga.dy "3"
                                    , Svga.fill "white"
                                    ]
                                    [ Svg.text <| formatTemp t ]
                        )
                )
            ]



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


makeSample : Float -> Int -> Sample
makeSample t s =
    ( t, Date.fromTime (second * toFloat s) )


sampleDecoder : Decode.Decoder Sample
sampleDecoder =
    Decode.map2 makeSample (Decode.index 0 Decode.float) (Decode.index 1 Decode.int)


decodeSamples : Decode.Decoder ProbeSamples
decodeSamples =
    Decode.keyValuePairs (Decode.list sampleDecoder)
