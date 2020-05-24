module TestLevel exposing (coordinate, suite, tuple2coordinate)

import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, intRange, tuple)
import Test exposing (Test, describe, fuzz)
import Types exposing (Coordinate)


tuple2coordinate : ( Int, Int ) -> Coordinate
tuple2coordinate ( row, col ) =
    { row = row, col = col }


coordinate : Fuzzer Coordinate
coordinate =
    Fuzz.map tuple2coordinate (tuple ( int, int ))


suite : Test
suite =
    Debug.todo "Add tests"
