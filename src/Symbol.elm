module Symbol exposing
    ( Symbol(..)
    , fromActor
    , randomMine
    , toString
    )

import Random
import Types exposing (Actor, BoardState(..), DoneState(..), Flag(..), Mine(..), PlayState(..))


randomMine : Random.Generator Mine
randomMine =
    Random.uniform A
        [ B
        , C
        , D
        , E
        , F
        , G
        , H
        , I
        , J
        , K
        , L
        , M
        , N
        , O
        ]


type Symbol
    = Mine Mine
    | Flag Flag
    | Incorrect
    | ExplodedMine
    | Disarmed Flag
    | Count Int
    | Board BoardState
    | Hamburger
    | Heart
    | Machine


fromActor : Actor -> Symbol
fromActor actor =
    case actor of
        Types.Robot ->
            Machine

        Types.Human ->
            Heart


toString : Symbol -> String
toString s =
    case s of
        Mine A ->
            "🤒"

        Mine B ->
            "😷"

        Mine C ->
            "🤮"

        Mine D ->
            "🤢"

        Mine E ->
            "🤡"

        Mine F ->
            "🧟"

        Mine G ->
            "🤥"

        Mine H ->
            "🤕"

        Mine I ->
            "🤧"

        Mine J ->
            "👻"

        Mine K ->
            "🥵"

        Mine L ->
            "🥶"

        Mine M ->
            "👹"

        Mine N ->
            "👺"

        Mine O ->
            "🦠"

        Flag Special ->
            "🇳🇴"

        Flag Normal ->
            "☣️"

        Flag Uncertain ->
            "❓"

        Incorrect ->
            "💩"

        Disarmed Normal ->
            "🥰"

        Disarmed _ ->
            "🥺"

        ExplodedMine ->
            "💀"

        Count num ->
            let
                tamil =
                    3046

                --thai =
                --    3664
                --
                --ascii =
                --    Char.toCode '0'
                map : Int -> Char
                map n =
                    Char.fromCode (tamil + n)

                --Char.toCode '０' + n
                digits : List Int
                digits =
                    String.fromInt num
                        |> String.toList
                        |> List.map (String.toInt << String.fromChar)
                        |> List.filterMap identity

                number =
                    String.fromList (List.map map digits)
            in
            if num < 0 then
                "-" ++ number

            else
                number

        Board boardState ->
            case boardState of
                Done GameOver ->
                    "🤬"

                Done Completed ->
                    "🏆"

                Playing (Paused _) ->
                    "🧘"

                NotInitialized ->
                    "🤫"

                Initialized ->
                    "🤟"

                Playing InProgress ->
                    "🎮"

                Demo ->
                    "🎯"

        Hamburger ->
            "🍔"

        Heart ->
            "❤️"

        Machine ->
            "🤖"
