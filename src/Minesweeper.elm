module Minesweeper exposing
    ( Board
    , Handler
    , Handlers
    , Minesweeper
    , PartialBoard
    , beginner
    , defaultHandlers
    , demoBoard
    , getBoard
    , mineRange
    , mkBoard
    , mkLevel
    , togglePause
    , update
    , view
    , viewCell
    )

import Array exposing (Array)
import Dict
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Html.Events.Extra.Mouse as Event
import Lib
    exposing
        ( between
        , bool2int
        , isEven
        , isOdd
        )
import Random exposing (Seed)
import Random.Set as Random
import Set exposing (Set)
import Svg
    exposing
        ( g
        , svg
        , text
        , text_
        , use
        )
import Svg.Attributes as A
import Svg.Lazy exposing (lazy3, lazy4)
import SvgHelper
    exposing
        ( cellSize
        , hexOffset
        , href
        )
import Symbol exposing (Symbol(..), randomMine)
import Types
    exposing
        ( BoardState(..)
        , Cell(..)
        , CellMsg(..)
        , CellState
        , Coordinate
        , DoneState(..)
        , Flag(..)
        , GridType(..)
        , Level
        , Mine(..)
        , Msg(..)
        , Revealed(..)
        , TimerEvent(..)
        , Topology(..)
        , doneState
        , getIndex
        )


type Minesweeper
    = Board Board



-- getBoardRecord : Minesweeper -> PartialBoard {}


getBoard : Minesweeper -> Board
getBoard (Board b) =
    b


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

        Done Completed ->
            "COMPLETED"

        Done GameOver ->
            "GAME_OVER"

        Demo ->
            "DEMO"


flag : Int -> Board -> ( Board, Maybe TimerEvent )
flag index board =
    ( if board.state /= Playing then
        board

      else
        case Array.get index board.cells of
            Nothing ->
                board

            Just cell ->
                updateCell board (flagCell board.useUncertainFlag cell)
    , Nothing
    )


updateCell : Board -> Cell -> Board
updateCell board cell =
    let
        coordinate =
            getIndex cell

        cells =
            Array.set coordinate cell board.cells

        stats =
            countStates cells

        newState =
            case cell of
                Exposed _ Exploded ->
                    if stats.exploded >= board.lives then
                        Done GameOver

                    else
                        Playing

                _ ->
                    Playing
    in
    { board
        | state = newState
        , cells = cells
        , stats = stats
    }


revealSingle : Int -> Cell -> Cell
revealSingle threats cell =
    case cell of
        New i (Just mine) ->
            Exposed i (Mined mine)

        New i _ ->
            Exposed i (Open threats)

        _ ->
            cell


mapThreats : Board -> Cell -> Int
mapThreats board =
    .mined << countNeighbourStates board


revealAll : Array Int -> Board -> ( Board, Maybe TimerEvent )
revealAll threatMap board =
    let
        mapCell cell =
            revealSingle (Maybe.withDefault -1 (Array.get (getIndex cell) threatMap)) cell
    in
    ( { board | cells = Array.map mapCell board.cells }
    , Just Stop
    )


gameWon : Board -> Bool
gameWon board =
    case board.state of
        Done Completed ->
            True

        Playing ->
            let
                { rows, cols } =
                    board.level

                { stats } =
                    board

                { mined, open } =
                    stats
            in
            open + mined == rows * cols

        _ ->
            False


revealNeighbours : Cell -> Board -> Board
revealNeighbours cell board =
    if board.state /= Playing then
        board

    else
        let
            mayPoke : Cell -> Bool
            mayPoke c =
                case c of
                    New _ _ ->
                        True

                    Flagged _ Uncertain _ ->
                        True

                    _ ->
                        False

            folder : Cell -> Board -> Board
            folder c b =
                Tuple.first (poke (getIndex c) b)
        in
        getNeighbours board cell
            -- Only poke new cells
            |> List.filter mayPoke
            |> List.foldl folder board


poke : Int -> Board -> ( Board, Maybe TimerEvent )
poke index board =
    if board.state /= Playing then
        ( board, Nothing )

    else
        case Array.get index board.cells of
            Nothing ->
                ( board, Nothing )

            Just cell ->
                let
                    threatMap : () -> Array Int
                    threatMap _ =
                        Array.map (mapThreats board) board.cells

                    neighbourMap : () -> Array ( Int, List Int )
                    neighbourMap _ =
                        Array.map
                            (\c ->
                                ( mapThreats board c, (List.map getIndex << getNeighbours board) c )
                            )
                            board.cells

                    newBoard =
                        let
                            { mined, exploded, flagged, flaggedUncertain } =
                                countNeighbourStates board cell
                        in
                        case cell of
                            Exposed _ (Open 0) ->
                                board

                            Exposed _ (Open _) ->
                                if flagged + exploded - flaggedUncertain == mined then
                                    revealNeighbours cell board

                                else
                                    board

                            New i Nothing ->
                                let
                                    numThreats =
                                        mapThreats board cell
                                in
                                if numThreats /= 0 then
                                    (updateCell board << revealSingle numThreats) cell

                                else
                                    let
                                        safeCells : Set Int
                                        safeCells =
                                            collectSafe board.level (neighbourMap ()) i Set.empty

                                        newCells =
                                            Array.map
                                                (\c ->
                                                    let
                                                        idx =
                                                            getIndex c
                                                    in
                                                    if Set.member idx safeCells then
                                                        (Exposed idx << Open << threats) idx

                                                    else
                                                        c
                                                )
                                                board.cells

                                        threatMap_ =
                                            threatMap ()

                                        threats n =
                                            Maybe.withDefault -1 (Array.get n threatMap_)
                                    in
                                    { board | cells = newCells, stats = countStates newCells }

                            New i m ->
                                let
                                    newCell =
                                        Exposed i
                                            (if m /= Nothing then
                                                Exploded

                                             else
                                                Open mined
                                            )

                                    updated =
                                        updateCell board newCell
                                in
                                if mined == 0 && m == Nothing then
                                    revealNeighbours cell updated

                                else
                                    updated

                            Flagged i Uncertain _ ->
                                let
                                    cmd : (a -> ( a, b )) -> a -> a
                                    cmd f b =
                                        Tuple.first (f b)
                                in
                                (cmd (poke i) << cmd (flag i)) board

                            _ ->
                                board

                    ( completed, gameOver ) =
                        doneState newBoard.state
                in
                if gameOver || completed then
                    revealAll (threatMap ()) newBoard

                else if gameWon newBoard then
                    revealAll (threatMap ()) { newBoard | state = Done Completed }

                else
                    ( newBoard, Nothing )


defaultPoker : Handler (Maybe CellMsg)
defaultPoker cell e =
    case e.button of
        Event.MainButton ->
            Just (GotPoked (getIndex cell))

        _ ->
            Nothing


defaultFlagger : Handler (Maybe CellMsg)
defaultFlagger cell _ =
    Just (GotFlagged (getIndex cell))


defaultHandlers : Maybe (Handlers (Maybe CellMsg))
defaultHandlers =
    Just
        { poker = defaultPoker
        , flagger = defaultFlagger
        }


update : Seed -> CellMsg -> Minesweeper -> ( Minesweeper, Maybe TimerEvent )
update seed msg ((Board board) as game) =
    let
        ( cmd, cell ) =
            case msg of
                GotPoked c ->
                    ( poke, c )

                GotFlagged c ->
                    ( flag, c )
    in
    case board.state of
        NotInitialized ->
            update seed msg (initialize seed game)

        Initialized ->
            (update seed msg << Board) { board | state = Playing }
                |> Tuple.mapSecond (always (Just Start))

        Playing ->
            Tuple.mapFirst Board (cmd cell board)

        Done GameOver ->
            ( Board board, Just Stop )

        Done Completed ->
            ( Board board, Just Stop )

        _ ->
            ( Board board, Nothing )


togglePause : Minesweeper -> ( Minesweeper, Maybe TimerEvent )
togglePause (Board b) =
    let
        { state } =
            b
    in
    case state of
        Playing ->
            ( Board { b | state = Paused }, Just Stop )

        Paused ->
            ( Board { b | state = Playing }, Just Start )

        _ ->
            ( Board b, Nothing )


type alias PartialBoard a =
    { a
        | useUncertainFlag : Bool
        , lives : Int
        , level : Level
    }


type alias Board =
    PartialBoard
        { cells : Array Cell
        , state : BoardState
        , stats : CellState Int
        }


emptyBoard =
    { cells = Array.empty
    , state = NotInitialized
    , lives = 3
    , useUncertainFlag = True
    , stats = mapCellState bool2int emptyCellState
    , level = beginner Hex Plane
    }


demoBoard : Topology -> GridType -> Minesweeper
demoBoard topology type_ =
    let
        (Board board) =
            mkBoard
                { emptyBoard
                    | level =
                        { rows = 4
                        , cols = 4
                        , topology = topology
                        , type_ = type_
                        , mines = 0
                        , useUncertainFlag = True
                        }
                }

        cell i _ =
            if i < 8 then
                Exposed i (Open (i + 1))

            else
                case i of
                    8 ->
                        Exposed i (Open 0)

                    9 ->
                        New i Nothing

                    10 ->
                        Flagged i Normal (Just A)

                    11 ->
                        Flagged i Uncertain (Just A)

                    12 ->
                        Exposed i (Mined O)

                    13 ->
                        Flagged i Special (Just A)

                    14 ->
                        Flagged i Normal Nothing

                    _ ->
                        Exposed i Exploded
    in
    Board
        { board
            | state = Demo
            , cells = Array.indexedMap cell board.cells
        }


mkBoard : PartialBoard a -> Minesweeper
mkBoard x =
    let
        level =
            mkLevel x.level
    in
    Board <|
        { emptyBoard
            | level = level
            , lives = x.lives
            , useUncertainFlag = x.useUncertainFlag
            , cells = Array.initialize (level.cols * level.rows) (\i -> New i Nothing)
        }


initialize : Seed -> Minesweeper -> Minesweeper
initialize seed (Board board) =
    Board
        { board
            | state = Initialized
            , lives = clamp 1 3 board.lives
            , cells = createCells board.level seed
            , level = board.level
        }


clampMines : Float -> Float
clampMines =
    clamp 0.1 0.4


mineRange : Level -> { min : Int, max : Int }
mineRange level =
    let
        min_ =
            numMines { level | mines = clampMines 0 }

        max_ =
            numMines { level | mines = clampMines 1 }
    in
    { min = min_, max = max_ }


numMines : Level -> Int
numMines level =
    round <| toFloat level.rows * toFloat level.cols * clampMines level.mines


createCells : Level -> Seed -> Array Cell
createCells level seed =
    let
        { rows, cols } =
            level

        mineSet =
            seed
                |> Random.step (Random.set (numMines level) (Random.int 0 (rows * cols - 1)))
                |> Tuple.first

        randomMine_ : a -> Int -> ( Int, Mine )
        randomMine_ _ i =
            ( i
            , Random.initialSeed i
                |> Random.step Random.independentSeed
                |> (Tuple.first >> Random.step randomMine >> Tuple.first)
            )

        mineDict : Dict.Dict Int Mine
        mineDict =
            Set.toList mineSet
                |> List.indexedMap randomMine_
                |> Dict.fromList

        cells =
            Array.initialize (rows * cols) (\i -> New i (Dict.get i mineDict))
    in
    cells



-- VIEW


type alias Handler msg =
    Cell -> Event.Event -> msg


type alias Handlers msg =
    { poker : Handler msg
    , flagger : Handler msg
    }


view : Maybe (Handlers msg) -> Minesweeper -> Html msg
view handlers (Board board) =
    let
        { state, level } =
            board

        offsets =
            case level.type_ of
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
                    case level.type_ of
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

        cells =
            case state of
                NotInitialized ->
                    Array.initialize (rows * cols) (\i -> New i Nothing)

                _ ->
                    board.cells

        { type_, topology, rows, cols } =
            level

        mapCell i cell =
            let
                { row, col } =
                    calculateCoordinate level.cols i
            in
            lazy3 mapSingleCell row col cell

        mapSingleCell : Int -> Int -> Cell -> Html msg
        mapSingleCell row col cell =
            let
                offset =
                    if isOdd row then
                        offsets.xOffset

                    else
                        0

                x_ =
                    String.fromFloat (toFloat col * cellSize + offset)

                y_ =
                    String.fromFloat (toFloat row * cellSize * offsets.yFactor)

                cellAttributes =
                    [ A.x x_
                    , A.y y_
                    , A.width (String.fromFloat cellSize)
                    , A.height (String.fromFloat cellSize)
                    ]
            in
            svg cellAttributes [ lazy4 viewCell handlers state type_ cell ]

        extraCells =
            case topology of
                Toroid ->
                    let
                        f cell =
                            let
                                c =
                                    calculateCoordinate level.cols <| getIndex cell

                                between_ a =
                                    between ( -2, a )
                            in
                            List.filterMap
                                (\( r_, c_ ) ->
                                    if between_ (rows + 1) r_ && between_ (cols + 1) c_ then
                                        Just (lazy3 mapSingleCell r_ c_ cell)

                                    else
                                        Nothing
                                )
                                [ ( c.row, c.col - cols )
                                , ( c.row, c.col + cols )
                                , ( c.row - rows, c.col )
                                , ( c.row + rows, c.col )
                                , ( c.row + rows, c.col + cols )
                                , ( c.row - rows, c.col - cols )
                                , ( c.row + rows, c.col - cols )
                                , ( c.row - rows, c.col + cols )
                                ]
                    in
                    Array.toList cells
                        |> List.filterMap
                            (\cell ->
                                let
                                    c =
                                        calculateCoordinate level.cols <| getIndex cell

                                    bl =
                                        between ( 0, 2 )

                                    pr =
                                        bl c.row || between ( rows - 1, rows - 3 ) c.row

                                    pc =
                                        bl c.col || between ( cols - 1, cols - 3 ) c.col
                                in
                                if pr || pc then
                                    Just <| f cell

                                else
                                    Nothing
                            )
                        |> List.concat

                _ ->
                    []

        cells_ =
            Array.toList cells
                |> List.indexedMap mapCell
                |> (++) extraCells

        attributes =
            [ A.preserveAspectRatio "xMidYMid meet"
            , A.class "SvgBoard"
            , attribute "data-grid" (gridTypeToString type_)
            , attribute "data-s" (boardState2String state)
            , A.viewBox (SvgHelper.viewBox viewBox_)
            ]
    in
    svg
        attributes
        [ g [] cells_ ]



-- Helpers


getNeighbours : Board -> Cell -> List Cell
getNeighbours x cell =
    let
        calculateIndex coordinate =
            coordinate.col + x.level.cols * coordinate.row
    in
    getNeighbourCoordinates (getIndex cell) x.level
        |> List.map (\i -> Array.get (calculateIndex i) x.cells)
        |> List.filterMap identity


countNeighbourStates : Board -> Cell -> CellState Int
countNeighbourStates board origin =
    getNeighbours board origin
        |> Array.fromList
        |> countStates


countStates : Array Cell -> CellState Int
countStates cells =
    let
        initial =
            mapCellState bool2int emptyCellState

        folder : Cell -> CellState Int -> CellState Int
        folder =
            liftState2 ((+) << bool2int) << cellState
    in
    cells
        |> Array.foldl folder initial


{-| infoMap Array[cell index] (threatCount, neighbours)
-}
collectSafe : Level -> Array ( Int, List Int ) -> Int -> Set Int -> Set Int
collectSafe level infoMap origin acc =
    let
        safe =
            Set.insert origin acc
    in
    case Array.get origin infoMap of
        -- 0 threats, safe to open neighbours
        Just ( 0, neighbours ) ->
            if Set.member origin acc then
                -- already visited
                safe

            else
                neighbours
                    |> List.filter (\x -> (not << Set.member x) acc)
                    |> List.foldl (collectSafe level infoMap) safe

        _ ->
            safe



-- LEVEL


maxDim : Int
maxDim =
    30


minDim : Int
minDim =
    -- must be even
    4


mkLevel : Level -> Level
mkLevel x =
    let
        cols =
            clamp minDim maxDim x.cols

        rows =
            let
                r =
                    if x.type_ == Hex && x.topology == Toroid && isOdd x.rows then
                        x.rows + 1

                    else
                        x.rows
            in
            clamp minDim maxDim r
    in
    { x
        | cols = cols
        , rows = rows
        , mines = clampMines x.mines
    }


getNeighbourCoordinates : Int -> Level -> List Coordinate
getNeighbourCoordinates index grid =
    let
        origin =
            calculateCoordinate grid.cols index

        { rows, cols, type_, topology } =
            grid

        neighbours =
            case type_ of
                Hex ->
                    hexNeighbours origin

                Square ->
                    squareNeighbours

        torusMap { row, col } =
            let
                -- https://github.com/elm/compiler/issues/1773
                -- Elm is unable to pattern match negative numbers in case of
                torusAdjust max n =
                    if n == -1 then
                        max - 1

                    else if n == max then
                        0

                    else
                        n
            in
            { row = torusAdjust rows row
            , col = torusAdjust cols col
            }

        addCoordinates_ =
            add origin
    in
    -- No need to filter out origin, since res never contains original coordinate
    case topology of
        Plane ->
            List.map addCoordinates_ neighbours
                |> List.filter (contains grid)

        Toroid ->
            List.map (torusMap << addCoordinates_) neighbours


beginner : GridType -> Topology -> Level
beginner g t =
    { rows = 6
    , cols = 10
    , type_ = g
    , topology = t
    , mines = 0.1
    , useUncertainFlag = True
    }



--intermediate : GridType -> Topology -> Level
--intermediate g t =
--    { rows = 16
--    , cols = 16
--    , type_ = g
--    , topology = t
--    , mines = 0.15625
--    , useUncertainFlag = True
--    }
--
--
--expert : GridType -> Topology -> Level
--expert g t =
--    { rows = 16
--    , cols = 30
--    , type_ = g
--    , topology = t
--    , mines = 0.20625
--    , useUncertainFlag = True
--    }
-- LEVEL HELPERS


add : Coordinate -> Coordinate -> Coordinate
add a b =
    { row = a.row + b.row, col = a.col + b.col }


hexNeighbours : Coordinate -> List Coordinate
hexNeighbours origin =
    if isEven origin.row then
        [ { row = 1, col = 0 }
        , { row = 1, col = -1 }
        , { row = 0, col = -1 }
        , { row = -1, col = -1 }
        , { row = -1, col = 0 }
        , { row = 0, col = 1 }
        ]

    else
        [ { row = 1, col = 1 }
        , { row = 1, col = 0 }
        , { row = 0, col = -1 }
        , { row = -1, col = 0 }
        , { row = -1, col = 1 }
        , { row = 0, col = 1 }
        ]


squareNeighbours : List Coordinate
squareNeighbours =
    [ { row = -1, col = -1 }
    , { row = -1, col = 0 }
    , { row = -1, col = 1 }
    , { row = 0, col = -1 }
    , { row = 0, col = 1 }
    , { row = 1, col = -1 }
    , { row = 1, col = 0 }
    , { row = 1, col = 1 }
    ]


calculateCoordinate : Int -> Int -> Coordinate
calculateCoordinate cols i =
    let
        col =
            remainderBy cols i

        row =
            (i - col) // cols
    in
    { row = row, col = col }


type alias Dim a =
    { a
        | rows : Int
        , cols : Int
    }


contains : Dim a -> Coordinate -> Bool
contains { rows, cols } { row, col } =
    between ( 0, rows - 1 ) row && between ( 0, cols - 1 ) col



-- CELL


mapCellState : (a -> b) -> CellState a -> CellState b
mapCellState f { flagged, flaggedUncertain, mined, exploded, new, revealed, open } =
    { flagged = f flagged
    , flaggedUncertain = f flaggedUncertain
    , mined = f mined
    , exploded = f exploded
    , new = f new
    , revealed = f revealed
    , open = f open
    }


applyState : CellState (b -> d) -> CellState b -> CellState d
applyState f { flagged, flaggedUncertain, mined, exploded, new, revealed, open } =
    { flagged = f.flagged flagged
    , flaggedUncertain = f.flaggedUncertain flaggedUncertain
    , mined = f.mined mined
    , exploded = f.exploded exploded
    , new = f.new new
    , revealed = f.revealed revealed
    , open = f.open open
    }


liftState2 : (a -> b -> b) -> CellState a -> CellState b -> CellState b
liftState2 f =
    applyState << mapCellState f


emptyCellState : CellState Bool
emptyCellState =
    { flagged = False
    , flaggedUncertain = False
    , mined = False
    , exploded = False
    , new = False
    , revealed = False
    , open = False
    }


cellState : Cell -> CellState Bool
cellState cell =
    case cell of
        New _ mine ->
            { emptyCellState
                | mined = mine /= Nothing
                , new = True
            }

        Exposed _ Exploded ->
            { emptyCellState
                | mined = True
                , exploded = True
                , revealed = True
            }

        Exposed _ (Mined _) ->
            { emptyCellState
                | mined = True
                , revealed = True
            }

        Exposed _ (Open _) ->
            { emptyCellState
                | open = True
                , revealed = True
            }

        Flagged _ f mine ->
            { emptyCellState
                | mined = mine /= Nothing
                , flagged = True
                , flaggedUncertain = f == Uncertain
            }


flagCell : Bool -> Cell -> Cell
flagCell useUncertain cell =
    case cell of
        New i mined ->
            Flagged i Normal mined

        Flagged i c mined ->
            if useUncertain then
                case c of
                    Normal ->
                        Flagged i Uncertain mined

                    _ ->
                        New i mined

            else
                New i mined

        Exposed _ _ ->
            cell



-- CELL VIEW


viewCell : Maybe (Handlers msg) -> BoardState -> GridType -> Cell -> Html msg
viewCell handlers boardState gridType cell =
    let
        { mined, open, flaggedUncertain } =
            cellState cell

        center =
            String.fromFloat (cellSize / 2)

        text__ t =
            text_
                (List.filterMap identity
                    [ Just (A.class "ct")
                    , Just (A.x center)
                    , Just (A.y center)
                    , Just (A.dominantBaseline "central")
                    , Just (A.textAnchor "middle")
                    , Just (A.fill "white")
                    , Just (A.fontSize center)
                    , if not open then
                        -- emoji
                        Just (attribute "role" "img")

                      else
                        Nothing
                    ]
                )
                [ text t ]

        symbol s =
            text__ (Symbol.toString s)

        ( state, content ) =
            case cell of
                New _ _ ->
                    ( 0, [ cover ] )

                Exposed _ (Mined m) ->
                    ( 0
                    , [ background
                      , symbol <|
                            if boardState == Done Completed || boardState == Demo then
                                Symbol.Disarmed Uncertain

                            else
                                Symbol.Mine m
                      ]
                    )

                Exposed _ (Open 0) ->
                    ( 1, [ background ] )

                Exposed _ (Open n) ->
                    ( 1, [ background, symbol (Count n) ] )

                Flagged _ f m ->
                    let
                        cssState =
                            if flaggedUncertain then
                                3

                            else
                                2

                        slab =
                            if completed || gameOver then
                                background

                            else
                                cover

                        ( s, el ) =
                            if (boardState == Demo || gameOver) && m == Nothing then
                                ( Symbol.Incorrect, background )

                            else if boardState == Demo && f == Special then
                                ( Symbol.Disarmed Normal, background )

                            else if completed then
                                ( Symbol.Disarmed Normal, slab )

                            else
                                ( Symbol.Flag f, slab )
                    in
                    ( cssState, [ el, symbol s ] )

                Exposed _ Exploded ->
                    ( 4, [ background, symbol ExplodedMine ] )

        viewBox =
            List.map String.fromFloat [ 0, 0, cellSize, cellSize ]
                |> String.join " "
                |> A.viewBox

        attributes_ =
            [ A.class "c"
            , viewBox
            , attribute "data-s" (String.fromInt state)
            ]
                ++ handlerAttrs

        handlerAttrs =
            case ( attachHandlers, handlers ) of
                ( True, Just { poker, flagger } ) ->
                    [ stop "mousedown" (poker cell), stop "contextmenu" (flagger cell) ]

                _ ->
                    []

        attachHandlers =
            not (completed || gameOver || boardState == Demo || boardState == Paused)

        stop e =
            { stopPropagation = True, preventDefault = True }
                |> Event.onWithOptions e

        optionalAttributes =
            [ if mined && (completed || gameOver || boardState == Demo) then
                Just (attribute "data-m" "t")

              else
                Nothing
            , case cell of
                Exposed _ (Open threats) ->
                    Just (attribute "data-t" (String.fromInt threats))

                _ ->
                    Nothing
            ]

        ( completed, gameOver ) =
            doneState boardState

        attributes =
            attributes_ ++ List.filterMap identity optionalAttributes

        gridType_ =
            "#" ++ gridTypeToString gridType

        cover =
            shape "cc"

        background =
            shape "cb"

        shape class =
            use [ href gridType_, A.class class ] []
    in
    svg attributes content


gridTypeToString : GridType -> String
gridTypeToString x =
    case x of
        Hex ->
            "HEX"

        Square ->
            "SQUARE"
