module Minesweeper exposing
    ( Board
    , Minesweeper
    , beginner
    , boardState2String
    , emptyCellState
    , expert
    , getState
    , intermediate
    , mapCellState
    , mkBoard
    , togglePause
    , update
    , view
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
        , flip
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
        , Mine
        , Msg(..)
        , Revealed(..)
        , RevealedState(..)
        , TimerEvent(..)
        , Topology(..)
        , doneState
        , getIndex
        )


type Minesweeper
    = Board Board



-- getBoardRecord : Minesweeper -> PartialBoard {}


getState : Minesweeper -> PartialBoard { state : BoardState, stats : CellState Int }
getState (Board b) =
    { lives = b.lives
    , state = b.state
    , seed = b.seed
    , stats = b.stats
    , level = b.level
    , useUncertainFlag = b.useUncertainFlag
    }


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
                    mapThreats : Cell -> Int
                    mapThreats =
                        .mined << countNeighbourStates board

                    threatMap : () -> Array Int
                    threatMap _ =
                        Array.map mapThreats board.cells

                    neighbourMap : () -> Array ( Int, List Int )
                    neighbourMap _ =
                        Array.map
                            (\c ->
                                ( mapThreats c, (List.map getIndex << getNeighbours board) c )
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
                                        mapThreats cell
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


update : CellMsg -> Minesweeper -> ( Minesweeper, Maybe TimerEvent )
update msg (Board board) =
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
            update msg (mkBoard board)

        Initialized ->
            (update msg << Board) { board | state = Playing }
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
        | seed : Seed
        , useUncertainFlag : Bool
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
    , seed = Random.initialSeed 37
    , state = NotInitialized
    , lives = 3
    , useUncertainFlag = True
    , stats = mapCellState bool2int emptyCellState
    , level = beginner Hex Plane
    }


mkBoard : PartialBoard a -> Minesweeper
mkBoard x =
    Board
        (initialize
            { emptyBoard
                | level = x.level
                , lives = x.lives
                , useUncertainFlag = x.useUncertainFlag
                , seed = x.seed
            }
        )


initialize : Board -> Board
initialize board =
    let
        { seed, lives } =
            board

        level =
            mkLevel board.level
    in
    { board
        | state = Initialized
        , lives = clamp 1 3 lives
        , cells = createCells level seed
        , seed = seed
        , level = level
    }


createCells : Level -> Seed -> Array Cell
createCells level seed =
    let
        { rows, cols, mines } =
            level

        mineSet =
            seed
                |> Random.step (Random.set mines (Random.int 0 (rows * cols - 1)))
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
            Array.repeat (rows * cols) 0
                |> Array.indexedMap (\i _ -> New i (Dict.get i mineDict))
    in
    cells



-- VIEW


view : Minesweeper -> Html Msg
view (Board board) =
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
            mapSingleCell (calculateCoordinate level i) cell

        mapSingleCell : Coordinate -> Cell -> Html Msg
        mapSingleCell { row, col } cell =
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
            svg cellAttributes [ viewCell state type_ cell ]

        extraCells =
            case topology of
                Toroid ->
                    let
                        f ( i, cell ) =
                            let
                                c =
                                    calculateCoordinate level i

                                between_ a =
                                    between ( -2, a )
                            in
                            List.filter (\( r_, c_ ) -> between_ (rows + 1) r_ && between_ (cols + 1) c_)
                                [ ( c.row, c.col - cols )
                                , ( c.row, c.col + cols )
                                , ( c.row - rows, c.col )
                                , ( c.row + rows, c.col )
                                , ( c.row + rows, c.col + cols )
                                , ( c.row - rows, c.col - cols )
                                , ( c.row + rows, c.col - cols )
                                , ( c.row - rows, c.col + cols )
                                ]
                                |> List.map
                                    (\( row_, col_ ) ->
                                        mapSingleCell { row = row_, col = col_ } cell
                                    )
                    in
                    Array.toList cells
                        |> List.indexedMap Tuple.pair
                        |> List.filter
                            (\( i, _ ) ->
                                let
                                    c =
                                        calculateCoordinate level i

                                    bl =
                                        between ( 0, 2 )

                                    pr =
                                        bl c.row || between ( rows - 1, rows - 3 ) c.row

                                    pc =
                                        bl c.col || between ( cols - 1, cols - 3 ) c.col
                                in
                                pr || pc
                            )
                        |> List.concatMap f

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
    getNeighbourCoordinates (getIndex cell) x.level
        |> List.map (\i -> Array.get (calculateIndex x.level i) x.cells)
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

        folder =
            mapStateAndLift bool2int (+)
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
    6


mkLevel : Level -> Level
mkLevel x =
    let
        minMines =
            ceiling (toFloat (rows * cols) * 0.1)

        maxMines =
            floor (toFloat (rows * cols) * 0.5)

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

        mines =
            clamp minMines maxMines x.mines
    in
    { x
        | cols = cols
        , rows = rows
        , mines = mines
    }


getNeighbourCoordinates : Int -> Level -> List Coordinate
getNeighbourCoordinates index grid =
    let
        origin =
            calculateCoordinate grid index

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
    , mines = 10
    }


intermediate : GridType -> Topology -> Level
intermediate g t =
    { rows = 16
    , cols = 16
    , type_ = g
    , topology = t
    , mines = 40
    }


expert : GridType -> Topology -> Level
expert g t =
    { rows = 16
    , cols = 30
    , type_ = g
    , topology = t
    , mines = 99
    }



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


calculateCoordinate : { a | cols : Int } -> Int -> Coordinate
calculateCoordinate { cols } i =
    let
        col =
            remainderBy cols i

        row =
            (i - col) // cols
    in
    { row = row, col = col }


calculateIndex : { a | cols : Int } -> Coordinate -> Int
calculateIndex { cols } coordinate =
    coordinate.col + cols * coordinate.row


type alias Dim a =
    { a
        | rows : Int
        , cols : Int
    }


contains : Dim a -> Coordinate -> Bool
contains { rows, cols } { row, col } =
    between ( 0, rows - 1 ) row && between ( 0, cols - 1 ) col



-- CELL


mapCellState : (Bool -> b) -> CellState Bool -> CellState b
mapCellState f { flagged, flaggedUncertain, mined, exploded, new, revealed, open } =
    { flagged = f flagged
    , flaggedUncertain = f flaggedUncertain
    , mined = f mined
    , exploded = f exploded
    , new = f new
    , revealed = f revealed
    , open = f open
    }


mapStateAndLift : (Bool -> b) -> (b -> b -> b) -> (Cell -> CellState b -> CellState b)
mapStateAndLift f g x y =
    liftState2 g ((flip mapCellState << cellState) x f) y


liftState2 : (a -> b -> b) -> (CellState a -> CellState b -> CellState b)
liftState2 f { flagged, flaggedUncertain, mined, exploded, new, revealed, open } x =
    { x
        | flagged = f flagged x.flagged
        , flaggedUncertain = f flaggedUncertain x.flaggedUncertain
        , mined = f mined x.mined
        , exploded = f exploded x.exploded
        , new = f new x.new
        , revealed = f revealed x.revealed
        , open = f open x.open
    }


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


viewCell : BoardState -> GridType -> Cell -> Html Msg
viewCell boardState gridType cell =
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
                            if boardState == Done Completed then
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
                            if gameOver && m == Nothing then
                                ( Symbol.Incorrect, background )

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

        attachHandlers =
            not (completed || gameOver || boardState == Demo || boardState == Paused)

        handleClick e =
            case e.button of
                Event.MainButton ->
                    Cell (GotPoked (getIndex cell))

                Event.SecondButton ->
                    Cell (GotFlagged (getIndex cell))

                _ ->
                    Relax

        optionalAttributes =
            [ if mined && (completed || gameOver) then
                Just (attribute "data-m" "t")

              else
                Nothing
            , case cell of
                Exposed _ (Open threats) ->
                    Just (attribute "data-t" (String.fromInt threats))

                _ ->
                    Nothing
            , if attachHandlers then
                Just (Event.onDown handleClick)

              else
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
