module TestGrid exposing (coordinate, isJust, suite, tuple2coordinate)

import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, intRange, tuple)
import Grid exposing (Coordinate, GridType(..), Topology(..), add, maxDim, minDim, mkGrid)
import Test exposing (Test, describe, fuzz, test)


tuple2coordinate : ( Int, Int ) -> Coordinate
tuple2coordinate ( row, col ) =
    { row = row, col = col }


coordinate : Fuzzer Coordinate
coordinate =
    Fuzz.map tuple2coordinate (tuple ( int, int ))


suite : Test
suite =
    describe "The Grid module"
        [ describe "Grid.addCoordinates"
            [ fuzz coordinate "{ row = 0, col = 0 }" <|
                \c ->
                    c
                        |> add { row = 0, col = 0 }
                        |> Expect.equal c
            , fuzz (tuple ( coordinate, coordinate )) "any" <|
                \( c1, c2 ) ->
                    let
                        f a b =
                            { row = a.row + b.row, col = a.col + b.col }
                    in
                    c1
                        |> add c2
                        |> Expect.equal (f c1 c2)
            ]
        , describe "Grid.calculate*"
            [ fuzz (tuple ( intRange minDim maxDim, intRange minDim maxDim )) "roundtrip" <|
                \x ->
                    let
                        cols =
                            c.col + 1

                        c =
                            tuple2coordinate x

                        i =
                            Grid.calculateIndex { cols = cols } c
                    in
                    Expect.equal c (Grid.calculateCoordinate { cols = cols } i)
            ]
        ]



-- Helpers


isJust : Maybe a -> Bool
isJust x =
    case x of
        Just _ ->
            True

        _ ->
            False
