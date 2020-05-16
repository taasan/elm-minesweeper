module Board exposing (Board, BoardRecord, boardState2String, countStates, emptyBoard, mkBoard, togglePause, update, view)

import Array exposing (Array)
import Cell exposing (Cell(..), Flag(..), Revealed(..))
import Dict
import Grid exposing (Coordinate, Grid, GridType(..), Topology(..), calculateCoordinate, calculateIndex, mkGrid)
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Lib exposing (between, bool2int, isOdd, onContextMenu, random)
import List.Extra as List
import List.Split as List
import Random exposing (Seed)
import Random.Array as Random
import Random.Extra as Random
import Random.Set as Random
import Set
import Svg exposing (g, svg)
import Svg.Attributes as A
import Svg.Events as E
import Symbol exposing (randomMine)
import Types exposing (BoardState(..), CellMsg(..), DoneState(..), Msg(..), TimerEvent(..))
import View.Svg as SvgHelper exposing (cellSize, hexOffset)



-- BOARD


type alias BoardRecord =
    { level : Grid
    , cells : Array Cell
    , seed : Seed
    , useUncertainFlag : Bool
    }


get : Int -> Board -> Cell
get i ( state, board ) =
    Maybe.withDefault Void
        (case state of
            NotInitialized ->
                Nothing

            _ ->
                Array.get i board.cells
        )


boardState2String : BoardState -> String
boardState2String state =
    case state of
        NotInitialized ->
            "NOT_INITIALIZED"

        Initialized ->
            "INITIALIZED"

        Playing ->
            "PLAYING"

        Paused ->
            "PAUSED"

        Done Completed _ ->
            "COMPLETED"

        Done GameOver _ ->
            "GAME_OVER"

        Demo ->
            "DEMO"


mkBoard : BoardRecord -> Board
mkBoard props =
    ( NotInitialized, { props | level = mkGrid props.level } )


type alias Board =
    ( BoardState, BoardRecord )


flag : Int -> Board -> ( Board, Maybe TimerEvent )
flag coordinate b =
    let
        ( state, board ) =
            b
    in
    ( case state of
        Playing ->
            let
                flagCell =
                    Cell.flag board.useUncertainFlag

                cell =
                    get coordinate ( Playing, board )
            in
            updateCell cell coordinate b <| flagCell cell

        _ ->
            b
    , Nothing
    )


updateCell : Cell -> Int -> Board -> Cell -> Board
updateCell oldCell coordinate b cell =
    let
        ( state, board ) =
            b

        { level } =
            board
    in
    if (Cell.cellState oldCell).exploded || state /= Playing then
        b

    else
        let
            ( newState, updatedBoard ) =
                case cell of
                    Revealed Exploded ->
                        let
                            { exploded } =
                                countStates board

                            s =
                                if exploded >= level.lives - 1 then
                                    Done GameOver Types.NotRevealed

                                else
                                    Playing
                        in
                        ( s, board )

                    _ ->
                        ( Playing, board )
        in
        ( newState, { updatedBoard | cells = Array.set coordinate cell board.cells } )


revealAll : BoardState -> BoardRecord -> ( Board, Maybe TimerEvent )
revealAll state board =
    case state of
        Done status Types.NotRevealed ->
            let
                { level } =
                    board

                { rows, cols } =
                    level

                mapCell x =
                    let
                        cell =
                            get x ( state, board )
                    in
                    case cell of
                        Cell.New True ->
                            let
                                ( randomInt, _ ) =
                                    Random.step random (Random.initialSeed x)

                                ( _, s ) =
                                    Random.step Random.independentSeed <| Random.initialSeed (randomInt + x)

                                ( mine, _ ) =
                                    Random.step randomMine <| s
                            in
                            Cell.Revealed <| Cell.Mined mine

                        Cell.New False ->
                            Cell.Revealed <| Cell.Open (countNeighbourStates board x).mined

                        Cell.Revealed _ ->
                            cell

                        _ ->
                            cell
            in
            ( ( Done status Types.Revealed
              , { board
                    | cells = Array.fromList <| List.map mapCell <| List.range 0 (rows * cols - 1)
                }
              )
            , Just End
            )

        _ ->
            ( ( state, board ), Nothing )


gameWon : Board -> Bool
gameWon ( state, board ) =
    case state of
        Done Completed _ ->
            True

        Playing ->
            let
                { rows, cols } =
                    board.level

                cellStates =
                    countStates board

                { mined, open } =
                    cellStates
            in
            open + mined == (rows * cols)

        _ ->
            False


revealNeighbours : Int -> Board -> Board
revealNeighbours coordinate board =
    let
        ( state, b ) =
            board
    in
    case state of
        Playing ->
            let
                cell x =
                    (Cell.cellState << get x) ( Playing, b )
            in
            getNeighbours board coordinate
                |> List.map (calculateIndex b.level)
                |> List.map (\x -> ( x, cell x ))
                -- Only poke new cells
                |> List.filter (\( _, { new, flaggedUncertain } ) -> new || flaggedUncertain)
                |> List.foldl (\( x, _ ) -> Tuple.first << poke x) board

        _ ->
            board


poke : Int -> Board -> ( Board, Maybe TimerEvent )
poke coordinate board =
    let
        ( state, b ) =
            board

        x =
            case state of
                Playing ->
                    let
                        cell =
                            get coordinate board

                        { mined, exploded, flagged, flaggedUncertain } =
                            countNeighbourStates b coordinate
                    in
                    case cell of
                        Revealed r ->
                            case r of
                                Open t ->
                                    case t of
                                        0 ->
                                            board

                                        _ ->
                                            if flagged + exploded - flaggedUncertain >= mined then
                                                revealNeighbours coordinate board

                                            else
                                                board

                                _ ->
                                    board

                        New m ->
                            let
                                newCell =
                                    Revealed
                                        (if m then
                                            Exploded

                                         else
                                            Open mined
                                        )

                                updated =
                                    updateCell cell coordinate board newCell
                            in
                            if mined == 0 && not m then
                                revealNeighbours coordinate updated

                            else
                                updated

                        Flagged Uncertain _ ->
                            let
                                f_ x_ =
                                    x_ coordinate >> Tuple.first

                                flagAfterPoke =
                                    f_ flag >> f_ poke
                            in
                            flagAfterPoke board

                        _ ->
                            board

                _ ->
                    board

        ( newState, newBoard ) =
            x

        ( completed, gameOver, revealed ) =
            doneState newState
    in
    if not revealed then
        if gameOver || completed then
            revealAll newState newBoard

        else if gameWon x then
            revealAll (Done Completed Types.NotRevealed) newBoard

        else
            ( x, Nothing )

    else
        ( x, Nothing )


doneState : BoardState -> ( Bool, Bool, Bool )
doneState state =
    case state of
        Done s r ->
            ( s == Completed, s == GameOver, r == Types.Revealed )

        _ ->
            ( False, False, False )


update : CellMsg -> Board -> ( Board, Maybe TimerEvent )
update msg board =
    let
        ( state, b ) =
            board

        cmd =
            case msg of
                GotPoked _ ->
                    poke

                GotFlagged _ ->
                    flag

        coordinate =
            case msg of
                GotPoked c ->
                    c

                GotFlagged c ->
                    c
    in
    case state of
        NotInitialized ->
            update msg <| initialize board

        _ ->
            case state of
                Initialized ->
                    let
                        ( x, _ ) =
                            update msg <| ( Playing, b )
                    in
                    ( x, Just Begin )

                Playing ->
                    cmd coordinate board

                Done GameOver _ ->
                    ( board, Just End )

                Done Completed _ ->
                    ( board, Just End )

                _ ->
                    ( board, Nothing )


togglePause : Board -> ( Board, Maybe TimerEvent )
togglePause board =
    let
        ( state, b ) =
            board
    in
    case state of
        Playing ->
            ( ( Paused, b ), Just Pause )

        Paused ->
            ( ( Playing, b ), Just Play )

        _ ->
            ( board, Nothing )


emptyBoard : BoardRecord
emptyBoard =
    { level = Grid.emptyGrid
    , cells = Array.fromList []
    , seed = Random.initialSeed 37
    , useUncertainFlag = False
    }


initialize : Board -> Board
initialize board =
    let
        ( state, props ) =
            board
    in
    case state of
        NotInitialized ->
            let
                { seed, level, useUncertainFlag } =
                    props

                { lives } =
                    level

                ( cells, newSeed ) =
                    createCells ( level, seed )

                board_ =
                    { emptyBoard
                        | level = { level | lives = lives }
                        , cells = cells
                        , seed = newSeed
                        , useUncertainFlag = useUncertainFlag
                    }
            in
            ( Initialized, board_ )

        _ ->
            board


createCells : ( Grid, Seed ) -> ( Array Cell, Seed )
createCells ( level, seed ) =
    let
        { rows, cols, mines } =
            level

        ( mineSet, newSeed ) =
            Random.step (Random.set mines (Random.int 0 (rows * cols - 1))) seed

        mineDict =
            Dict.fromList <| List.indexedMap (\i c -> ( c, i )) <| Set.toList mineSet

        cells =
            Array.indexedMap
                (\i _ -> Cell.New (Dict.get i mineDict /= Nothing))
            <|
                Array.repeat (rows * cols) 0
    in
    ( cells, newSeed )



-- VIEW


type alias Handlers msg =
    { doPoke : Int -> msg
    , doFlag : Int -> msg
    }


view : Handlers msg -> Board -> Html msg
view { doPoke, doFlag } board =
    let
        ( boardState, b ) =
            board

        ( completed, gameOver, _ ) =
            doneState boardState

        attachHandlers =
            not (completed || gameOver || boardState == Demo || boardState == Paused)

        offsets =
            case level.gridType of
                Square ->
                    { xOffset = 0
                    , yFactor = 1
                    , width = cellSize * toFloat cols
                    , height = cellSize * toFloat rows
                    }

                Hex ->
                    let
                        xOffset =
                            cellSize / 2

                        yFactor =
                            hexOffset
                    in
                    { xOffset = xOffset
                    , yFactor = hexOffset
                    , width = cellSize * toFloat cols + xOffset
                    , height = ((toFloat rows - 1) * yFactor + 1) * cellSize
                    }

        viewBox_ =
            case level.topology of
                Plane ->
                    { x = 0
                    , y = 0
                    , width = offsets.width
                    , height = offsets.height
                    }

                Toroid ->
                    case level.gridType of
                        Hex ->
                            { x = -cellSize * 1.5
                            , y = -cellSize * 1.25
                            , width = offsets.width + cellSize * 2.5
                            , height = offsets.height + cellSize * 2.5
                            }

                        Square ->
                            { x = -cellSize * 2
                            , y = -cellSize * 2
                            , width = offsets.width + cellSize * 4
                            , height = offsets.height + cellSize * 4
                            }

        ( level, cells ) =
            case boardState of
                NotInitialized ->
                    let
                        l =
                            b.level
                    in
                    ( l, Cell.New False |> Array.repeat (l.rows * l.cols) )

                _ ->
                    ( b.level, b.cells )

        { gridType, topology, rows, cols } =
            level

        mapCell i cell =
            let
                coordinate =
                    calculateCoordinate level i
            in
            mapSingleCell coordinate cell i

        mapSingleCell : Coordinate -> Cell -> Int -> Html msg
        mapSingleCell { row, col } cell coordinate =
            let
                x_ =
                    String.fromFloat <|
                        toFloat col
                            * cellSize
                            + (if isOdd row then
                                offsets.xOffset

                               else
                                0
                              )

                y_ =
                    String.fromFloat <| toFloat row * cellSize * offsets.yFactor

                exploded =
                    (Cell.cellState cell).exploded

                handlers =
                    if attachHandlers && not exploded then
                        [ E.onClick (doPoke coordinate)
                        , onContextMenu (doFlag coordinate)
                        ]

                    else
                        []

                cellAttributes =
                    List.append handlers
                        [ A.x x_
                        , A.y y_
                        , A.width <| String.fromFloat cellSize
                        , A.height <| String.fromFloat cellSize
                        ]
            in
            svg cellAttributes [ Cell.view boardState gridType cell ]

        extraCells =
            let
                f ( i, cell ) =
                    let
                        c =
                            calculateCoordinate level i

                        between_ a =
                            between ( -2, a )
                    in
                    ( i
                    , ( cell
                      , List.filter (\( r_, c_ ) -> between_ (rows + 1) r_ && between_ (cols + 1) c_)
                            [ ( c.row, c.col - cols )
                            , ( c.row, c.col - cols )
                            , ( c.row, c.col + cols )
                            , ( c.row - rows, c.col )
                            , ( c.row + rows, c.col )
                            , ( c.row + rows, c.col + cols )
                            , ( c.row - rows, c.col - cols )
                            , ( c.row + rows, c.col - cols )
                            , ( c.row - rows, c.col + cols )
                            ]
                            |> List.map (\( row_, col_ ) -> mapSingleCell { row = row_, col = col_ } cell i)
                      )
                    )
            in
            case topology of
                Toroid ->
                    Array.toList cells
                        |> List.indexedMap Tuple.pair
                        |> List.filter
                            (\( i, _ ) ->
                                let
                                    c =
                                        calculateCoordinate level i

                                    pr =
                                        between ( 0, 2 ) c.row || between ( rows - 1, rows - 3 ) c.row

                                    pc =
                                        between ( 0, 2 ) c.col || between ( cols - 1, cols - 3 ) c.col
                                in
                                pr || pc
                            )
                        |> List.map f
                        |> List.concatMap (\( _, ( _, res ) ) -> res)

                _ ->
                    []

        cells_ =
            Array.toList cells
                |> List.indexedMap mapCell
                |> List.append extraCells

        attributes =
            [ A.preserveAspectRatio "xMidYMid meet"
            , A.class "SvgBoard"
            , attribute "data-grid" <| Grid.toString gridType
            , attribute "data-s" <| boardState2String <| boardState
            , A.viewBox <| SvgHelper.viewBox viewBox_
            ]
    in
    svg
        attributes
        [ g [] cells_ ]



-- Helpers


getNeighbours : Board -> Int -> List Coordinate
getNeighbours b coordinate =
    case b of
        ( NotInitialized, _ ) ->
            []

        ( _, board ) ->
            Grid.getNeighbours coordinate board.level


countNeighbourStates : BoardRecord -> Int -> Cell.State Int
countNeighbourStates board origin =
    neighbourStates board origin
        |> List.map (Cell.map bool2int)
        |> List.foldl (Cell.lift2 (+)) (Cell.map bool2int (Cell.cellState Void))


neighbourStates : BoardRecord -> Int -> List (Cell.State Bool)
neighbourStates board origin =
    Grid.getNeighbours origin board.level
        |> List.map (calculateIndex board.level)
        |> List.map (\i -> get i ( Playing, board ))
        |> List.map Cell.cellState


countStates : BoardRecord -> Cell.State Int
countStates board =
    let
        states : Array (Cell.State Int)
        states =
            Array.map (Cell.map bool2int) (getStates board)
    in
    Array.foldl (Cell.lift2 (+)) (Cell.map bool2int (Cell.cellState Void)) states


getStates : BoardRecord -> Array (Cell.State Bool)
getStates board =
    board.cells |> Array.map Cell.cellState
