module Msg exposing (Msg(..))

--
--

import Browser.Dom as Dom
import Http
import Model exposing (ProbeSamples)
import Time exposing (Time)


type Msg
    = Tick Time
    | NewSamples (Result Http.Error ProbeSamples)
    | NewWinSize (Result () Dom.Viewport)
