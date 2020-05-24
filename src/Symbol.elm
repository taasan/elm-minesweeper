module Symbol exposing
    ( Symbol(..)
    , randomMine
    , toString
    )

import Random
import Types
    exposing
        ( BoardState(..)
        , DoneState(..)
        , Flag(..)
        , Mine(..)
        )


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
    | ExplodedMine
    | Disarmed Flag
    | Count Int
    | Board BoardState
    | Hamburger


toString : Symbol -> String
toString s =
    case s of
        -- 🤒
        Mine A ->
            "\u{1F912}"

        -- 😷
        Mine B ->
            "😷"

        -- 🤮
        Mine C ->
            "\u{1F92E}"

        -- 🤢
        Mine D ->
            "\u{1F922}"

        -- 🤡
        Mine E ->
            "\u{1F921}"

        -- 🧟
        Mine F ->
            "\u{1F9DF}"

        -- 🤥
        Mine G ->
            "\u{1F925}"

        -- 🤕
        Mine H ->
            "\u{1F915}"

        -- 🤧
        Mine I ->
            "\u{1F927}"

        -- 👻
        Mine J ->
            "👻"

        -- 🥵
        Mine K ->
            "\u{1F975}"

        -- 🥶
        Mine L ->
            "\u{1F976}"

        -- 👹
        Mine M ->
            "👹"

        -- 👺
        Mine N ->
            "👺"

        -- 🦠
        Mine O ->
            "\u{1F9A0}"

        -- 🇳🇴
        Flag Special ->
            "🇳🇴"

        -- ☣️
        Flag Normal ->
            "☣️"

        -- ❓
        Flag Uncertain ->
            "❓"

        -- 💩
        Flag Incorrect ->
            "💩"

        -- 🥰
        Disarmed Normal ->
            "\u{1F970}"

        -- 🥺
        Disarmed _ ->
            "\u{1F97A}"

        -- 💀
        ExplodedMine ->
            "💀"

        Count n ->
            let
                c =
                    String.fromChar <| Char.fromCode <| Char.toCode '０' + n
            in
            String.fromInt n

        Board boardState ->
            case boardState of
                Done GameOver _ ->
                    -- 🤬
                    "\u{1F92C}"

                Paused ->
                    -- 🧘
                    "\u{1F9D8}"

                _ ->
                    -- 🎯
                    "🎯"

        Hamburger ->
            -- "🍔"
            "🍔"
