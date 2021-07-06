module Minesweeper exposing
    (  Board
       -- Comment to prevent the language server from making this list a one-liner

    , Minesweeper
    , PartialBoard
    , beginner
    , cellState
    , demoBoard
    , getBoard
    , initialize
    , mineRange
    , mkBoard
    , mkLevel
    , numMines
    , paused
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
import Html.Lazy exposing (lazy5)
import Lib
    exposing
        ( between
        , bool2int
        , isEven
        , isOdd
        )
import Player
    exposing
        ( Handlers
          --
        , Player
        , PlayerInfo
        )
import Random exposing (Seed)
import Random.Set as Random
import Set exposing (Set)
import Svg
    exposing
        ( g
          --
        , svg
        , text
        , text_
        , use
        )
import Svg.Attributes as A exposing (height, width)
import Svg.Lazy exposing (lazy3)
import SvgHelper
    exposing
        ( cellSize
          --
        , hexOffset
        , href
        )
import Symbol
    exposing
        ( randomMine
        )
import Types
    exposing
        ( BoardEntry
          --
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
        )


type Minesweeper
    = Board Board


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


flag : Maybe Flag -> Player msg -> Set Int -> Board -> ( Board, Maybe TimerEvent )
flag f _ cells board =
    let
        flag_ : Int -> Array BoardEntry -> Array BoardEntry
        flag_ i es =
            es
                |> Array.update i
                    (\e ->
                        let
                            cell m =
                                case f of
                                    Just x ->
                                        Flagged i x m

                                    _ ->
                                        Covered i m
                        in
                        { e
                            | cell =
                                case e.cell of
                                    Covered _ m ->
                                        cell m

                                    Flagged _ _ m ->
                                        cell m

                                    _ ->
                                        e.cell
                        }
                    )

        newEntries =
            Set.foldr flag_ board.entries cells
    in
    ( { board | entries = newEntries }, Nothing )


revealSingle : BoardEntry -> BoardEntry
revealSingle ({ threats, cell } as entry) =
    case cell of
        Covered i (Just mine) ->
            { entry | cell = Exposed i (Mined mine) }

        Covered i _ ->
            { entry | cell = Exposed i (Uncovered threats) }

        _ ->
            entry


revealAll : Board -> ( Board, Maybe TimerEvent )
revealAll board =
    ( { board | entries = Array.map revealSingle board.entries }
    , Just Stop
    )


revealNeighbours : PlayerInfo msg -> Level -> Int -> Array BoardEntry -> Array BoardEntry
revealNeighbours player level index entries =
    let
        mayPoke : Int -> Bool
        mayPoke c =
            case Maybe.map .cell <| Array.get c entries of
                Just (Covered _ _) ->
                    True

                Just (Flagged _ Uncertain _) ->
                    True

                _ ->
                    False

        folder : Int -> Array BoardEntry -> Array BoardEntry
        folder =
            pokeCell level player

        neighbours =
            (Maybe.withDefault [] << Maybe.map .neighbours) <| Array.get index entries
    in
    neighbours
        -- Only poke new cells
        |> List.filter mayPoke
        |> List.foldl folder entries


pokeCell : Level -> PlayerInfo msg -> Int -> Array BoardEntry -> Array BoardEntry
pokeCell level player index entries =
    case Array.get index entries of
        Nothing ->
            entries

        Just ({ threats, cell } as entry) ->
            let
                updateEntry x =
                    Array.set index x entries

                expose mined =
                    if mined then
                        Exposed index Exploded

                    else
                        Exposed index (Uncovered threats)
            in
            case entry.cell of
                Covered _ Nothing ->
                    if threats == 0 && player.autoRevealSafe then
                        revealSafe index { level = level, entries = entries }

                    else
                        updateEntry { entry | cell = expose False }

                Covered _ (Just _) ->
                    updateEntry { entry | cell = expose True }

                Flagged _ Uncertain m ->
                    updateEntry { entry | cell = expose (m /= Nothing) }

                Flagged _ _ m ->
                    if player.flagBlockPoke then
                        entries

                    else
                        updateEntry { entry | cell = expose (m /= Nothing) }

                Exposed _ (Uncovered _) ->
                    if player.revealNeighboursOnPoke then
                        let
                            { flagged, exploded, flaggedUncertain, mined } =
                                countNeighbourStates entries index
                        in
                        if flagged + exploded - flaggedUncertain == mined then
                            revealNeighbours player level index entries

                        else
                            entries

                    else
                        entries

                _ ->
                    entries


paused : Minesweeper -> Bool
paused (Board b) =
    Types.paused b.state


poke : Player msg -> Set Int -> Board -> ( Board, Maybe TimerEvent )
poke p cells board =
    let
        player =
            Player.info p

        newEntries =
            Set.foldr (pokeCell board.level player) board.entries cells

        newStats =
            countStates newEntries

        newState =
            if newStats.exploded >= player.lives then
                Done GameOver

            else if newStats.uncovered + newStats.mined == Array.length newEntries then
                Done Completed

            else
                board.state

        newBoard =
            { board
                | entries = newEntries
                , stats = newStats
                , state = newState
            }

        ( completed, gameOver ) =
            doneState newBoard.state
    in
    if player.revealAllOnGameOver && gameOver || completed then
        revealAll newBoard

    else
        ( newBoard, Nothing )


update : Player msg -> Seed -> CellMsg -> Minesweeper -> ( Minesweeper, Maybe TimerEvent )
update p seed msg ((Board board) as game) =
    let
        ( cmd, cell ) =
            case msg of
                GotPoked c ->
                    ( poke p, c )

                GotFlagged c ->
                    ( flag (Just Normal) p, c )

                GotUnflagged c ->
                    ( flag Nothing p, c )
    in
    case board.state of
        NotInitialized ->
            ( game, Nothing )

        -- update p seed msg (initialize seed game)
        Initialized ->
            (update p seed msg << Board) { board | state = Playing InProgress }
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
    case b.state of
        Playing InProgress ->
            ( Board { b | state = Playing <| Paused pauseState }, Just Stop )

        Playing (Paused _) ->
            ( Board { b | state = Playing InProgress }, Just Start )

        _ ->
            ( Board b, Nothing )


type alias PartialBoard a =
    { a | level : Level }


type alias Board =
    PartialBoard
        { entries : Array BoardEntry
        , state : BoardState
        , stats : CellState Int
        , seed : Maybe Seed
        }


emptyBoard : Board
emptyBoard =
    { entries = Array.empty
    , state = NotInitialized
    , stats = mapCellState bool2int emptyCellState
    , level = beginner Hex Plane
    , seed = Nothing
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
                        }
                }

        cell i =
            if i < 8 then
                Exposed i (Uncovered (i + 1))

            else
                case i of
                    8 ->
                        Exposed i (Uncovered 0)

                    9 ->
                        Covered i Nothing

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
            { cell = Covered i Nothing, threats = 0, neighbours = [] }
    in
    Board <|
        { emptyBoard
            | level = level
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
                    Array.initialize dim (\i -> Covered i (Dict.get i mineDict))

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
                            getNeighbours { level = level, entries = cells } i
                                |> List.map (\x -> { cell = x })
                                |> Array.fromList
                                |> countStates
                    in
                    { cell = c
                    , threats = neighbourStates.mined
                    , neighbours = (List.map getIndex << getNeighbours { level = level, entries = cells }) i
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
    in
    Board
        { b
            | entries = entries
            , seed = Just seed
            , state = Initialized
            , level = level
        }


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


view : Maybe (Handlers msg) -> Minesweeper -> Html msg
view handlers game =
    let
        board =
            getBoard game

        state =
            board.state

        level =
            board.level

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
                    Array.initialize (rows * cols) (\i -> Covered i Nothing)

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
            svg cellAttributes [ lazy5 viewCell Nothing handlers state type_ cell ]

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


countNeighbourStates : Array BoardEntry -> Int -> CellState Int
countNeighbourStates entries origin =
    let
        getNeighbours i =
            case Array.get i entries of
                Just { neighbours } ->
                    List.filterMap (flip Array.get entries) neighbours

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


revealSafe : Int -> { a | level : Level, entries : Array BoardEntry } -> Array BoardEntry
revealSafe i board =
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
                                Exposed (getIndex e.cell) (Uncovered e.threats)

                            else
                                e.cell
                    }
                )
                board.entries
    in
    newEntries


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
mapCellState f { flagged, flaggedUncertain, mined, exploded, covered, revealed, uncovered } =
    { flagged = f flagged
    , flaggedUncertain = f flaggedUncertain
    , mined = f mined
    , exploded = f exploded
    , covered = f covered
    , revealed = f revealed
    , uncovered = f uncovered
    }


applyState : CellState (b -> d) -> CellState b -> CellState d
applyState f { flagged, flaggedUncertain, mined, exploded, covered, revealed, uncovered } =
    { flagged = f.flagged flagged
    , flaggedUncertain = f.flaggedUncertain flaggedUncertain
    , mined = f.mined mined
    , exploded = f.exploded exploded
    , covered = f.covered covered
    , revealed = f.revealed revealed
    , uncovered = f.uncovered uncovered
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
    , covered = False
    , revealed = False
    , uncovered = False
    }


cellState : Cell -> CellState Bool
cellState cell =
    case cell of
        Covered _ mine ->
            { emptyCellState
                | mined = mine /= Nothing
                , covered = True
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

        Exposed _ (Uncovered _) ->
            { emptyCellState
                | uncovered = True
                , revealed = True
            }

        Flagged _ f mine ->
            { emptyCellState
                | mined = mine /= Nothing
                , flagged = True
                , flaggedUncertain = f == Uncertain
            }



-- CELL VIEW


viewCell : Maybe { a | width : Int, height : Int } -> Maybe (Handlers msg) -> BoardState -> GridType -> Cell -> Html msg
viewCell dim handlers boardState gridType cell =
    let
        { mined, uncovered, flaggedUncertain } =
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
                    , if not uncovered then
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
                Covered _ _ ->
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

                Exposed _ (Uncovered 0) ->
                    ( 1, [ background ] )

                Exposed _ (Uncovered n) ->
                    ( 1, [ background, symbol (Symbol.Count n) ] )

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
                    ( 4, [ background, symbol Symbol.ExplodedMine ] )

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
                    [ poker cell, flagger cell ]

                _ ->
                    []

        attachHandlers =
            not (completed || gameOver || boardState == Demo || Types.paused boardState)

        optionalAttributes =
            [ if mined && (completed || gameOver || boardState == Demo) then
                Just (attribute "data-m" "t")

              else
                Nothing
            , case cell of
                Exposed _ (Uncovered threats) ->
                    Just (attribute "data-t" (String.fromInt threats))

                _ ->
                    Nothing
            , Maybe.map
                (\d -> (width << String.fromInt) d.width)
                dim
            , Maybe.map
                (\d -> (height << String.fromInt) d.height)
                dim
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
