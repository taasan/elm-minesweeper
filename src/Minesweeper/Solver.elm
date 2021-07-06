module Minesweeper.Solver exposing (CellInfo, Msg, Solver)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed)
import Set exposing (Set)


type alias Field a =
    { a
        | rows : Int
        , cols : Int
        , mines : Int
        , cells : Array (Maybe CellInfo)
    }


type Solver
    = Solver
        (Field
            { done : Bool
            , frontier : List Int
            , probabilities : Array Float
            , seed : Seed
            }
        )


mkSolver : { a | rows : Int, cols : Int, mines : Int, seed : Seed } -> Maybe Solver
mkSolver field =
    let
        valid =
            List.all (\x -> x > 1) [ field.mines, field.cols, field.rows ]
                && List.all (\x -> x <= 30) [ field.cols, field.rows ]
                && (field.mines < field.cols * field.rows)
    in
    if valid then
        { cells = Array.initialize (field.cols * field.rows) (always Nothing)
        , frontier = []
        , probabilities = Array.initialize (field.cols * field.rows) (always <| toFloat field.mines / (toFloat field.cols * toFloat field.rows))
        , rows = field.rows
        , cols = field.cols
        , mines = field.mines
        , done = False
        , seed = field.seed
        }
            |> (Just << Solver)

    else
        Nothing


type alias CellInfo =
    { index : Int
    , mined : Bool
    , threats : Int
    , neighbours : Set Int
    }


onCellsUncovered : List CellInfo -> Solver -> Maybe Solver
onCellsUncovered xs (Solver solver) =
    if solver.done then
        Nothing

    else
        (Just << List.foldl uncover (Solver solver)) xs


uncover : CellInfo -> Solver -> Solver
uncover ({ index, mined, threats, neighbours } as info) (Solver solver) =
    if mined then
        Solver { solver | done = True }

    else
        case Array.get index solver.cells of
            Just Nothing ->
                let
                    ns : List ( Int, Maybe CellInfo )
                    ns =
                        List.map (\i -> ( i, Array.get i cells |> Maybe.andThen identity )) (Set.toList neighbours)

                    cells =
                        Array.set index (Just info) solver.cells

                    getProbability : Int -> Float
                    getProbability i =
                        case Array.get i solver.cells of
                            Just _ ->
                                if index == i then
                                    if mined then
                                        1

                                    else
                                        0

                                else
                                    toFloat threats / (toFloat << Set.size) neighbours

                            --Array.get i solver.probabilities
                            --    |> Maybe.withDefault -1
                            Nothing ->
                                Array.get i solver.probabilities
                                    |> Maybe.withDefault -1

                    fff : ( Int, Maybe CellInfo ) -> Array Float -> Array Float
                    fff maybeInfo cs =
                        case maybeInfo of
                            ( i, _ ) ->
                                Array.set i (getProbability i) cs

                    newProbs =
                        List.foldl fff solver.probabilities (( index, Just info ) :: ns)
                in
                Solver
                    { solver
                        | probabilities = newProbs
                        , cells = cells
                    }

            Just _ ->
                -- Already uncovered
                Solver solver

            Nothing ->
                -- Out of bounds
                Solver solver


main =
    Browser.sandbox
        { init = mkSolver { rows = 10, cols = 10, mines = 10, seed = Random.initialSeed 17 }
        , update = update
        , view = view
        }


type Msg
    = CellsUpdated (List CellInfo)


update : Msg -> Maybe Solver -> Maybe Solver
update msg maybeSolver =
    Maybe.andThen
        (\solver ->
            case msg of
                CellsUpdated xs ->
                    onCellsUncovered xs solver
        )
        maybeSolver


view : Maybe Solver -> Html Msg
view maybeSolver =
    case maybeSolver of
        Nothing ->
            text "Nothing to see"

        Just (Solver solver) ->
            div []
                [ button [ onClick <| CellsUpdated [ { index = 0, mined = False, threats = 0, neighbours = Set.fromList [ 1, 10 ] } ] ] [ text "Click me!" ]
                , Array.map (\x -> li [] [ text x ]) solver.probabilities
                    |> Array.toList
                    |> ul []
                ]
