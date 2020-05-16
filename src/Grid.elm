module Grid exposing (Coordinate, Grid, GridType(..), Topology(..), add, beginner, calculateCoordinate, calculateIndex, contains, emptyGrid, expert, getNeighbours, intermediate, maxDim, minDim, mkGrid, toString)

import Lib exposing (between, isEven)



-- Grid


type GridType
    = Hex
    | Square


toString : GridType -> String
toString x =
    case x of
        Hex ->
            "HEX"

        Square ->
            "SQUARE"


type alias Grid =
    { cols : Int
    , rows : Int
    , topology : Topology
    , gridType : GridType
    , mines : Int
    , lives : Int
    }


emptyGrid : Grid
emptyGrid =
    { cols = 0
    , rows = 0
    , topology = Plane
    , gridType = Square
    , mines = 0
    , lives = 0
    }


maxDim : Int
maxDim =
    30


minDim : Int
minDim =
    7


mkGrid : Grid -> Grid
mkGrid x =
    let
        minMines =
            ceiling <| (toFloat <| rows * cols) * 0.1

        maxMines =
            floor <| (toFloat <| rows * cols) * 0.5

        cols =
            clamp minDim maxDim x.cols

        rows =
            clamp minDim maxDim x.rows

        mines =
            clamp minMines maxMines x.mines

        lives =
            clamp 0 3 x.mines
    in
    { x
        | cols = cols
        , rows = rows
        , mines = mines
        , lives = lives
    }



--mkGrid : Grid -> Maybe Grid
--mkGrid x =
--    let
--        f : Int -> Bool
--        f =
--            not << between ( minDim, maxDim )
--    in
--    if f x.cols || f x.rows then
--        Nothing
--
--    else
--        Just x


type Topology
    = Plane
    | Toroid


type alias Coordinate =
    { row : Int
    , col : Int
    }


getNeighbours : Int -> Grid -> List Coordinate
getNeighbours index grid =
    let
        origin =
            calculateCoordinate grid index

        { rows, cols, gridType, topology } =
            grid

        neighbours =
            case gridType of
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

        addCoordinates_ a b =
            add a <| mapNzpCoordinate b

        res =
            case topology of
                Plane ->
                    List.map (addCoordinates_ origin) neighbours
                        |> List.filter (contains grid)

                Toroid ->
                    List.map torusMap <| List.map (addCoordinates_ origin) neighbours
    in
    -- No need to filter, since res never contains original coordinate
    -- |> List.filter (\x -> index /= calculateIndex grid x)
    res


beginner : GridType -> Topology -> Grid
beginner g t =
    { rows = 6
    , cols = 10
    , gridType = g
    , topology = t
    , mines = 10
    , lives = 3
    }


intermediate : GridType -> Topology -> Grid
intermediate g t =
    { rows = 16
    , cols = 16
    , gridType = g
    , topology = t
    , mines = 40
    , lives = 3
    }


expert : GridType -> Topology -> Grid
expert g t =
    { rows = 16
    , cols = 30
    , gridType = g
    , topology = t
    , mines = 0
    , lives = 3
    }



-- HELPERS


add : Coordinate -> Coordinate -> Coordinate
add a b =
    { row = a.row + b.row, col = a.col + b.col }


type NZP
    = N -- -1
    | Z --  0
    | P --  1


hexNeighbours : Coordinate -> List { row : NZP, col : NZP }
hexNeighbours origin =
    if isEven origin.row then
        [ { row = P, col = Z }
        , { row = P, col = N }
        , { row = Z, col = N }
        , { row = N, col = N }
        , { row = N, col = Z }
        , { row = Z, col = P }
        ]

    else
        [ { row = P, col = P }
        , { row = P, col = Z }
        , { row = Z, col = N }
        , { row = N, col = Z }
        , { row = N, col = P }
        , { row = Z, col = P }
        ]


squareNeighbours : List { row : NZP, col : NZP }
squareNeighbours =
    [ { row = N, col = N }
    , { row = N, col = Z }
    , { row = N, col = P }
    , { row = Z, col = N }
    , { row = Z, col = P }
    , { row = P, col = N }
    , { row = P, col = Z }
    , { row = P, col = P }
    ]


mapNzpCoordinate : { row : NZP, col : NZP } -> Coordinate
mapNzpCoordinate a =
    let
        n2c x =
            case x of
                N ->
                    -1

                Z ->
                    0

                P ->
                    1
    in
    { row = n2c a.row, col = n2c a.col }


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



-- type Address a
--     = Index Int
--     | RowCol Coordinate
--
--
-- contains : Dim a -> Address b -> Bool
-- contains dim a =
--     let
--         { rows, cols } =
--             dim
--     in
--     case a of
--         RowCol { row, col } ->
--             row < 0 || col < 0 || row >= rows || col >= cols
--
