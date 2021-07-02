module LevelChooser exposing (view)

import Basics.Extra exposing (uncurry)
import Html exposing (..)
import Html.Attributes as A
    exposing
        ( attribute
        , checked
        , class
        , name
        , step
        , title
        , type_
        , value
        )
import Html.Events exposing (..)
import Html.Lazy exposing (lazy2)
import Json.Decode as Json
import Lib exposing (combine)
import Minesweeper exposing (mineRange, viewCell)
import Types
    exposing
        ( BoardState(..)
        , Cell(..)
        , DoneState(..)
        , Flag(..)
        , GameMsg(..)
        , GridType(..)
        , Level
        , Msg(..)
        , PlayState(..)
        , Topology(..)
        )


onReset : msg -> Attribute msg
onReset msg =
    preventDefaultOn "reset" (Json.map alwaysPreventDefault (Json.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


view : Level -> Html (Maybe Msg)
view level =
    let
        typeRadio type_ =
            let
                checked_ =
                    level.type_ == type_
            in
            div [] [ viewEmptyCell type_ checked_ ]
                |> radio "GridTypeRadio" "type" checked_ (typeChanged type_)

        topologyRadio topology =
            let
                checked_ =
                    level.topology == topology

                content =
                    case topology of
                        Toroid ->
                            "🍩"

                        Plane ->
                            "🍦"
            in
            radio "TopologyRadio" "topology" checked_ (topologyChanged topology) (text content)

        typeChanged x =
            GotLevel { level | type_ = x }
                |> Just

        topologyChanged x =
            GotLevel { level | topology = x }
                |> Just

        setRows x =
            { level | rows = x }

        setCols x =
            { level | cols = x }

        setMines x =
            { level | mines = x }

        numberChanged convert f val =
            convert val
                |> Maybe.andThen (Just << GotLevel << Minesweeper.mkLevel << f)

        mineRange_ =
            mineRange level

        mineStep =
            (0.3 / toFloat (mineRange_.max.count - mineRange_.min.count))
                |> String.fromFloat
                |> String.left 6
    in
    div
        [ class "LevelChooser" ]
        [ form [ onSubmit <| Just <| Game <| RandomGame level, onReset <| Just PopPage ]
            [ div [ class "LevelChooser__controls" ]
                [ fieldset [] (List.map typeRadio [ Hex, Square ])
                , fieldset [] (List.map topologyRadio [ Plane, Toroid ])
                , fieldset []
                    [ button [ type_ "submit" ] [ text "👍" ]
                    , button [ type_ "reset" ] [ text "👎" ]
                    ]
                ]
            , fieldset []
                [ slider "1" String.fromInt 6 30 "Rows" level.rows (numberChanged String.toInt setRows)
                , slider "1" String.fromInt 6 30 "Columns" level.cols (numberChanged String.toInt setCols)
                , slider mineStep String.fromFloat mineRange_.min.ratio mineRange_.max.ratio "Mines" level.mines (numberChanged String.toFloat setMines)
                ]
            , div [ class "LevelChooser__demos" ] (demos level)
            ]
        ]


demos level =
    combine [ Plane, Toroid ] [ Hex, Square ]
        |> List.map (uncurry <| viewDemo level)


viewDemo : Level -> Topology -> GridType -> Html (Maybe Msg)
viewDemo level topology type_ =
    let
        selected =
            if level.topology == topology && level.type_ == type_ then
                Just (attribute "data-selected" "")

            else
                Nothing

        attributes =
            List.filterMap identity [ Just (class "SvgBoard__demo"), selected ]
    in
    div attributes
        [ Minesweeper.demoBoard topology type_
            |> lazy2 Minesweeper.view Nothing
        ]


viewEmptyCell : GridType -> Bool -> Html (Maybe a)
viewEmptyCell t flagged =
    let
        cell =
            if flagged then
                Flagged 0 Normal (Just Types.A)

            else
                New 0 Nothing
    in
    viewCell Nothing (Playing InProgress) t cell


radio : String -> String -> Bool -> msg -> Html msg -> Html msg
radio class_ name_ isChecked onInput_ content =
    label [ class "RadioGroup__Option" ]
        [ input
            [ class class_
            , type_ "radio"
            , checked isChecked
            , name name_
            , onInput (always onInput_)
            ]
            []
        , content
        ]


slider : String -> (number -> String) -> number -> number -> String -> number -> (String -> msg) -> Html msg
slider step_ toString max_ min_ text_ value_ onInput_ =
    if max_ < min_ then
        slider step_ toString min_ max_ text_ value_ onInput_

    else
        let
            val =
                clamp min_ max_ value_
                    |> toString
        in
        label []
            [ text text_
            , span
                [ attribute "aria-label" val
                , class "FormatNumber"
                , attribute "data-numeralsystem" "ascii"
                , title val
                ]
                []
            , input
                [ type_ "range"
                , A.max (toString max_)
                , A.min (toString min_)
                , step step_
                , onInput onInput_

                -- Attribute order matters
                -- https://github.com/Matt-Esch/virtual-dom/issues/228
                , value val
                ]
                []
            ]
