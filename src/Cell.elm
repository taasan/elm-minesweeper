module Cell exposing (Cell(..), Flag(..), Revealed(..), State, cellState, flag, lift2, map, view)

-- CELL

import Grid exposing (GridType)
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (svg, text, text_, use)
import Svg.Attributes as A
import Symbol exposing (Mine, Symbol(..))
import Types exposing (BoardState(..), DoneState(..))
import View.Svg exposing (cellSize, href)


type alias State a =
    { flagged : a
    , flaggedUncertain : a
    , mined : a
    , exploded : a
    , new : a
    , revealed : a
    , open : a
    }


map : (a -> b) -> State a -> State b
map f { flagged, flaggedUncertain, mined, exploded, new, revealed, open } =
    { flagged = f flagged
    , flaggedUncertain = f flaggedUncertain
    , mined = f mined
    , exploded = f exploded
    , new = f new
    , revealed = f revealed
    , open = f open
    }


lift2 : (a -> b -> b) -> (State a -> State b -> State b)
lift2 f { flagged, flaggedUncertain, mined, exploded, new, revealed, open } x =
    { x
        | flagged = f flagged x.flagged
        , flaggedUncertain = f flaggedUncertain x.flaggedUncertain
        , mined = f mined x.mined
        , exploded = f exploded x.exploded
        , new = f new x.new
        , revealed = f revealed x.revealed
        , open = f open x.open
    }


cellState : Cell -> State Bool
cellState cell =
    let
        x =
            { flagged = False
            , flaggedUncertain = False
            , mined = False
            , exploded = False
            , new = False
            , revealed = False
            , open = False
            }
    in
    case cell of
        New mined ->
            { x
                | mined = mined
                , new = True
            }

        Revealed Exploded ->
            { x
                | mined = True
                , exploded = True
                , revealed = True
            }

        Revealed (Mined _) ->
            { x
                | mined = True
                , revealed = True
            }

        Revealed (Open _) ->
            { x
                | open = True
                , revealed = True
            }

        Flagged f mined ->
            { x
                | mined = mined
                , flagged = True
                , flaggedUncertain = f == Uncertain
            }

        Void ->
            x


type Revealed
    = Open Int
    | Exploded
    | Mined Mine


type Flag
    = Normal
    | Uncertain


type Cell
    = New Bool
    | Revealed Revealed
    | Flagged Flag Bool
    | Void


flag : Bool -> Cell -> Cell
flag useUncertain cell =
    case cell of
        New mined ->
            Flagged Normal mined

        Flagged c mined ->
            if useUncertain then
                case c of
                    Normal ->
                        Flagged Uncertain mined

                    Uncertain ->
                        New mined

            else
                New mined

        _ ->
            cell



-- VIEW


view : BoardState -> GridType -> Cell -> Html msg
view boardState gridType cell =
    let
        { mined, open } =
            --, threats } =
            cellState cell

        center =
            String.fromFloat <| cellSize / 2

        text__ t =
            text_
                (List.filterMap identity
                    [ Just <| A.class "ct"
                    , Just <| A.x center
                    , Just <| A.y center
                    , Just <| A.dominantBaseline "central"
                    , Just <| A.textAnchor "middle"
                    , Just <| A.fill "white"
                    , Just <| A.fontSize center
                    , if not open then
                        -- emoji
                        Just <| attribute "role" "img"

                      else
                        Nothing
                    ]
                )
                [ text t ]

        symbol s =
            text__ <| Symbol.toString s

        ( state, content ) =
            case cell of
                Void ->
                    ( 0, [ cover ] )

                New _ ->
                    ( 0, [ cover ] )

                Revealed (Mined m) ->
                    ( 0
                    , [ background
                      , symbol <|
                            if boardState == Done Completed Types.Revealed then
                                Symbol.Disarmed Symbol.Uncertain

                            else
                                Symbol.Mine m
                      ]
                    )

                Revealed (Open 0) ->
                    ( 1, [ background ] )

                Revealed (Open n) ->
                    ( 1, [ background, text__ <| String.fromInt n ] )

                Flagged Normal m ->
                    let
                        ( s, el ) =
                            if boardState == Done GameOver Types.Revealed && not m then
                                ( Symbol.Flag Symbol.Incorrect, background )

                            else if boardState == Done Completed Types.Revealed then
                                ( Symbol.Disarmed Symbol.Flagged, background )

                            else
                                ( Symbol.Flag Symbol.Flagged, cover )
                    in
                    ( 2, [ el, symbol <| s ] )

                Flagged Uncertain _ ->
                    ( 3, [ cover, symbol <| Symbol.Flag Symbol.Uncertain ] )

                Revealed Exploded ->
                    ( 4, [ background, symbol <| Symbol.ExplodedMine ] )

        viewBox =
            A.viewBox <| String.join " " <| List.map String.fromFloat [ 0, 0, cellSize, cellSize ]

        attributes_ =
            [ A.class "c"
            , viewBox
            , attribute "data-s" <| String.fromInt state
            ]

        optionalAttributes =
            [ if mined && open then
                Just <| attribute "data-m" "t"

              else
                Nothing
            , case cell of
                Revealed (Open threats) ->
                    Just <| attribute "data-t" <| String.fromInt threats

                _ ->
                    Nothing
            ]

        attributes =
            List.append attributes_ <| List.filterMap identity optionalAttributes

        gridType_ =
            "#" ++ Grid.toString gridType

        cover =
            shape "cc"

        background =
            shape "cb"

        shape class =
            use [ href gridType_, A.class class ] []
    in
    svg attributes content
