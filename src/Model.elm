module Model exposing (Sample, ProbeSamples, Model, decodeSamples)

import Date
import Json.Decode as Decode
import Time exposing (second)


type alias Sample =
    ( Float, Date.Date )


type alias ProbeSamples =
    List ( String, List Sample )


type alias Model =
    { probeData : ProbeSamples
    , mobile : Bool
    }


makeSample : Float -> Int -> Sample
makeSample t s =
    ( t, Date.fromTime (second * toFloat s) )


sampleDecoder : Decode.Decoder Sample
sampleDecoder =
    Decode.map2 makeSample (Decode.index 0 Decode.float) (Decode.index 1 Decode.int)


decodeSamples : Decode.Decoder ProbeSamples
decodeSamples =
    Decode.keyValuePairs (Decode.list sampleDecoder)
