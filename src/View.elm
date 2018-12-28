module View exposing (view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class)
import Model exposing (..)
import Msg exposing (Msg)
import Round
import String exposing (join)
import Svg
import Svg.Attributes as Svga
import Time



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
            (List.map (\( key, samples ) -> card key samples model.mobile) model.probeData)
        ]
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


card : String -> List Sample -> Bool -> Html Msg
card key samples mobile =
    case List.head samples of
        Just ( temp, date ) ->
            div [ class "Card" ]
                [ h2 [] [ text <| locate key ]
                , div [ class "subtitle" ] [ text <| formatTime date ]
                , div [ class "temperature" ] [ text <| formatTemp temp ++ "°" ]
                , graph 360 230 samples
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
