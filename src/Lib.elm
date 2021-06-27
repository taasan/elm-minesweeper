module Lib exposing
    ( between
    , bool2int
    , flip
    , getSeed
    , isEven
    , isOdd
    , message
    )

import Bitwise exposing (and)
import Random exposing (Generator)
import Task
import Time exposing (Posix)
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


flip : (c -> b -> a) -> b -> c -> a
flip f x y =
    f y x


getSeed : Posix -> Msg
getSeed p =
    p
        |> Time.posixToMillis
        |> Random.initialSeed
        |> GotSeed


message : Msg -> Cmd Msg
message msg =
    Task.perform (always msg) (Task.succeed ())
