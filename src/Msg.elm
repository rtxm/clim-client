module Msg exposing (Msg(..))

--
--

import Browser.Dom as Dom
import Http
import Model exposing (ProbeSamples)
import Time exposing (Posix)


type Msg
    = Tick Posix
    | NewSamples (Result Http.Error ProbeSamples)
    | NewWinSize Int Int
