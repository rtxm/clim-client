module View exposing (view)

import Browser exposing (Document)
import Time

import Element exposing (Element, el, text, column, row, fill, width, height, rgb255, spacing, centerX, padding, html, px)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Round
import String exposing (join)
import Svg
import Svg.Attributes as Svga

import Model exposing (..)
import Msg exposing (Msg)


-- VIEW


sansSerif : List Font.Font
sansSerif =
    [ Font.typeface "helvetica"
    , Font.typeface "arial"
    , Font.sansSerif
    ]

styleCard =
    [ Font.color (rgb255 0 0 0)
    , Background.color (rgb255 114 159 207)
    , Font.family sansSerif
    , Font.size 16
    ]

styleHeading =
    [ Font.family sansSerif
    , Font.size 32
    ]

styleSubTitle =
    [ Font.size 24
    , Font.color (rgb255 32 32 32)
    ]

styleTemperature =
    [ Font.size 96
    , Font.color (rgb255 255 255 255)
    , centerX
    ]


view : Model -> Document Msg
view model =
    let
        container =
            if model.mobile then
                column [ spacing 20 ]

            else
                row [ spacing 20, padding 20 ]
    in
    { title = "Temperadur er gêr"

      , body = [Element.layout [] <|
            container
                (List.map (\( key, samples ) -> card key samples model.mobile) model.probeData)]
    }

formatTime : Time.Posix -> String
formatTime date =
    let
        hour =
            String.fromInt (Time.toHour Time.utc date)

        minutes =
            String.fromInt (Time.toMinute Time.utc date)
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


card : String -> List Sample -> Bool -> Element Msg
card key samples mobile =
    case List.head samples of
        Just ( temp, date ) ->
            column
                ([ width
                    (if mobile then
                        fill

                     else
                        px 400
                    )
                , height (if mobile then fill else px 500)
                ] ++ styleCard)
                [ el styleHeading  (text <| locate key)
                , el styleSubTitle (text <| formatTime date)
                , el styleTemperature  (text <| formatTemp temp ++ "°")
                , html (graph 360 230 samples)
                ]

        Nothing ->
            Element.none


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


graph : Float -> Float -> List Sample -> Svg.Svg msg
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
                |> List.indexedMap (\x y -> (projX_ <| toFloat x) ++ "," ++ projY_ y)
                |> join " "
    in
    Svg.svg
        [ Svga.viewBox ("0 0 " ++ String.fromFloat gWidth ++ " " ++ String.fromFloat gHeight) ]
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
                                    [ Svg.text <| formatTime d ]
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
