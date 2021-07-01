module Lib exposing
    ( between
    , bool2int
    , combine
    , getSeed
    , isEven
    , isOdd
    )

import Bitwise exposing (and)
import Random
import Task
import Time
import Types exposing (Msg(..))


isEven : Int -> Bool
isEven n =
    and n 1 == 0


isOdd : Int -> Bool
isOdd =
    not << isEven


between : ( Int, Int ) -> Int -> Bool
between ( a, b ) x =
    let
        a_ =
            min a b

        b_ =
            max a b
    in
    x >= a_ && x <= b_


bool2int : Bool -> Int
bool2int b =
    if b then
        1

    else
        0


getSeed : Task.Task x Random.Seed
getSeed =
    Task.andThen (\time -> Task.succeed (Random.initialSeed (Time.posixToMillis time))) Time.now


combine : List a -> List b -> List ( a, b )
combine xs ys =
    apply (List.map Tuple.pair xs) ys


apply : List (a -> b) -> List a -> List b
apply fs xs =
    List.concatMap (\f -> List.map f xs) fs
