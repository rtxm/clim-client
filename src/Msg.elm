module Msg exposing (..)

import Time exposing (Time)


--

import Http
import Window


--

import Model exposing (ProbeSamples)


type Msg
    = Tick Time
    | NewSamples (Result Http.Error ProbeSamples)
    | NewWinSize Window.Size
