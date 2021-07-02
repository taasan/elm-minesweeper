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
import Array.Extra as Array
import Basics.Extra exposing (flip)
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
        ( BoardEntry
        , BoardState(..)
        , Cell(..)
        , CellMsg(..)
        , CellState
        , ChangeMethod
        , Coordinate
        , DoneState(..)
        , Flag(..)
        , GridType(..)
        , Level
        , Mine(..)
        , Msg(..)
        , PlayState(..)
        , Revealed(..)
        , TimerEvent(..)
        , Topology(..)
        , doneState
        , getIndex
        , inProgress
        , paused
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

        Playing InProgress ->
            "PLAYING"

        Playing (Paused _) ->
            "PAUSED"

        Done Completed ->
            "COMPLETED"

        Done GameOver ->
            "GAME_OVER"

        Demo ->
            "DEMO"


flag : Int -> Board -> ( Board, Maybe TimerEvent )
flag index board =
    ( if not <| inProgress board.state then
        board

      else
        case Array.get index board.entries of
            Nothing ->
                board

            Just entry ->
                updateCell board (flagCell board.useUncertainFlag entry)
    , Nothing
    )


updateCell : Board -> { a | cell : Cell } -> Board
updateCell board { cell } =
    let
        coordinate =
            getIndex cell

        entries =
            Array.update coordinate (\c -> { c | cell = cell }) board.entries

        stats =
            countStates entries

        newState =
            case cell of
                Exposed _ Exploded ->
                    if stats.exploded >= board.lives then
                        Done GameOver

                    else
                        board.state

                _ ->
                    board.state
    in
    { board
        | state = newState
        , entries = entries
        , stats = stats
    }


revealSingle : BoardEntry -> BoardEntry
revealSingle ({ threats, cell } as entry) =
    case cell of
        New i (Just mine) ->
            { entry | cell = Exposed i (Mined mine) }

        New i _ ->
            { entry | cell = Exposed i (Open threats) }

        _ ->
            entry


revealAll : Board -> ( Board, Maybe TimerEvent )
revealAll board =
    ( { board | entries = Array.map revealSingle board.entries }
    , Just Stop
    )


gameWon : Board -> Bool
gameWon board =
    case board.state of
        Done Completed ->
            True

        Playing InProgress ->
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


revealNeighbours : Int -> Board -> Board
revealNeighbours index board =
    if not <| inProgress board.state then
        board

    else
        let
            mayPoke : Int -> Bool
            mayPoke c =
                case Maybe.map .cell <| Array.get c board.entries of
                    Just (New _ _) ->
                        True

                    Just (Flagged _ Uncertain _) ->
                        True

                    _ ->
                        False

            folder : Int -> Board -> Board
            folder i b =
                Tuple.first (poke i b)

            getNeighbours =
                case Array.get index board.entries of
                    Nothing ->
                        []

                    Just { neighbours } ->
                        neighbours
        in
        getNeighbours
            -- Only poke new cells
            |> List.filter mayPoke
            |> List.foldl folder board


poke : Int -> Board -> ( Board, Maybe TimerEvent )
poke index board =
    if not <| inProgress board.state then
        ( board, Nothing )

    else
        case Array.get index board.entries of
            Nothing ->
                ( board, Nothing )

            Just ({ threats, cell } as entry) ->
                let
                    newBoard =
                        let
                            { mined, exploded, flagged, flaggedUncertain } =
                                countNeighbourStates board index
                        in
                        case cell of
                            Exposed _ (Open 0) ->
                                board

                            Exposed _ (Open _) ->
                                if flagged + exploded - flaggedUncertain == mined then
                                    revealNeighbours (getIndex entry.cell) board

                                else
                                    board

                            New i Nothing ->
                                if threats /= 0 then
                                    (updateCell board << revealSingle) entry

                                else
                                    let
                                        safeCells : Set Int
                                        safeCells =
                                            collectSafe board.level board.entries i Set.empty

                                        newEntries : Array BoardEntry
                                        newEntries =
                                            Array.map
                                                (\e ->
                                                    { e
                                                        | cell =
                                                            let
                                                                idx =
                                                                    getIndex e.cell
                                                            in
                                                            if Set.member idx safeCells then
                                                                Exposed (getIndex e.cell) (Open e.threats)

                                                            else
                                                                e.cell
                                                    }
                                                )
                                                board.entries
                                    in
                                    { board | entries = newEntries, stats = countStates newEntries }

                            New i (Just _) ->
                                updateCell board { cell = Exposed i Exploded }

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
                    revealAll newBoard

                else if gameWon newBoard then
                    revealAll { newBoard | state = Done Completed }

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
            (update seed msg << Board) { board | state = Playing InProgress }
                |> Tuple.mapSecond (always (Just Start))

        Playing InProgress ->
            Tuple.mapFirst Board (cmd cell board)

        Done GameOver ->
            ( Board board, Just Stop )

        Done Completed ->
            ( Board board, Just Stop )

        _ ->
            ( Board board, Nothing )


togglePause : ChangeMethod -> Minesweeper -> ( Minesweeper, Maybe TimerEvent )
togglePause pauseState (Board b) =
    let
        { state } =
            b
    in
    case state of
        Playing InProgress ->
            ( Board { b | state = Playing <| Paused pauseState }, Just Stop )

        Playing (Paused _) ->
            ( Board { b | state = Playing InProgress }, Just Start )

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
        { entries : Array BoardEntry
        , state : BoardState
        , stats : CellState Int
        }


emptyBoard =
    { entries = Array.empty
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

        cell i =
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
            , entries = Array.indexedMap (\i e -> { e | cell = cell i }) board.entries
        }


mkBoard : PartialBoard a -> Minesweeper
mkBoard x =
    let
        level =
            mkLevel x.level

        emptyEntry : Int -> BoardEntry
        emptyEntry i =
            { cell = New i Nothing, threats = 0, neighbours = [] }
    in
    Board <|
        { emptyBoard
            | level = level
            , lives = x.lives
            , useUncertainFlag = x.useUncertainFlag
            , entries = Array.initialize (level.cols * level.rows) emptyEntry
        }


initialize : Seed -> Minesweeper -> Minesweeper
initialize seed (Board b) =
    let
        level =
            b.level

        dim =
            level.rows * level.cols

        entries =
            let
                cells =
                    Array.initialize dim (\i -> New i (Dict.get i mineDict))

                calculateIndex coordinate =
                    coordinate.col + level.cols * coordinate.row

                getNeighbours x origin =
                    getNeighbourCoordinates origin x.level
                        |> List.map (\i -> Array.get (calculateIndex i) x.entries)
                        |> List.filterMap identity
            in
            Array.indexedMap
                (\i c ->
                    let
                        neighbourStates =
                            getNeighbours { level = board.level, entries = cells } i
                                |> List.map (\x -> { cell = x })
                                |> Array.fromList
                                |> countStates
                    in
                    { cell = c
                    , threats = neighbourStates.mined
                    , neighbours = (List.map getIndex << getNeighbours { level = board.level, entries = cells }) i
                    }
                )
            <|
                cells

        mineSet =
            seed
                |> Random.step (Random.set (numMines level) (Random.int 0 (dim - 1)))
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

        board =
            { b
                | state = Initialized
                , lives = clamp 1 3 b.lives
                , level = level
            }
    in
    Board { board | entries = entries }


clampMines : Float -> Float
clampMines =
    clamp 0.1 0.25


mineRange : Level -> { min : { count : Int, ratio : Float }, max : { count : Int, ratio : Float } }
mineRange level =
    let
        f x =
            { count = numMines { level | mines = x }, ratio = x }
    in
    { min = f (clampMines 0), max = f (clampMines 1) }


numMines : Level -> Int
numMines level =
    round <|
        toFloat level.rows
            * toFloat level.cols
            * clampMines level.mines



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
                    Array.map .cell board.entries

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
            Array.indexedMapToList mapCell cells
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


countNeighbourStates : Board -> Int -> CellState Int
countNeighbourStates board origin =
    let
        getNeighbours i =
            case Array.get i board.entries of
                Just { neighbours } ->
                    List.filterMap (flip Array.get board.entries) neighbours

                _ ->
                    []
    in
    (countStates << Array.fromList << getNeighbours) origin


countStates : Array { a | cell : Cell } -> CellState Int
countStates entries =
    let
        initial =
            mapCellState bool2int emptyCellState

        folder =
            liftState2 ((+) << bool2int) << cellState << .cell
    in
    entries
        |> Array.foldl folder initial


collectSafe : Level -> Array BoardEntry -> Int -> Set Int -> Set Int
collectSafe level entries origin acc =
    let
        safe =
            Set.insert origin acc
    in
    case Array.get origin entries of
        Just { threats, neighbours } ->
            let
                isNotVisited x =
                    (not << Set.member x) acc
            in
            if threats == 0 && isNotVisited origin then
                neighbours
                    |> List.filter isNotVisited
                    |> List.foldl (collectSafe level entries) safe

            else
                safe

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


flagCell : Bool -> { a | cell : Cell } -> { a | cell : Cell }
flagCell useUncertain entry =
    let
        cell =
            case entry.cell of
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
                    entry.cell
    in
    { entry | cell = cell }



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
            not (completed || gameOver || boardState == Demo || paused boardState)

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
