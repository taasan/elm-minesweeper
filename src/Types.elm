module Types exposing
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
    , RevealedState(..)
    , Shade(..)
    , Theme
    , TimerEvent(..)
    , Topology(..)
    , doneState
    , getIndex
    , isDone
    , isWon
    )

import Random exposing (Seed)
import Time



-- MESSAGE


type Msg
    = Cell CellMsg
    | TogglePause
    | GotBlurred
    | NewGame
    | SetTheme String
    | RandomGame
    | GotSeed Seed
    | GotCurrentTime Time.Posix
    | GotTimerEvent TimerEvent Time.Posix
    | Relax
    | Recv String


type CellMsg
    = GotFlagged Cell
    | GotPoked Cell



-- BOARD


type DoneState
    = Completed
    | GameOver


type RevealedState
    = Revealed
    | NotRevealed


type BoardState
    = NotInitialized
    | Initialized
    | Playing
    | Paused
    | Done DoneState RevealedState
    | Demo



-- LEVEL


type GridType
    = Hex
    | Square


type alias Level =
    { cols : Int
    , rows : Int
    , topology : Topology
    , type_ : GridType
    , mines : Int
    }


type Topology
    = Plane
    | Toroid


type alias Coordinate =
    { row : Int
    , col : Int
    }



-- CELL


type alias CellState a =
    { flagged : a
    , flaggedUncertain : a
    , mined : a
    , exploded : a
    , new : a
    , revealed : a
    , open : a
    }


type Revealed
    = Open Int
    | Exploded
    | Mined Mine


type Flag
    = Normal
    | Uncertain
    | Special
    | Incorrect


type Cell
    = New Int (Maybe Mine)
    | Exposed Int Revealed
    | Flagged Int Flag (Maybe Mine)


type Mine
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O


getIndex : Cell -> Int
getIndex cell =
    case cell of
        New i _ ->
            i

        Exposed i _ ->
            i

        Flagged i _ _ ->
            i



-- THEME


type Shade
    = Dark
    | Light


type alias Theme =
    { name : String
    , classes : List String
    , variant : Shade
    }


isDone : BoardState -> Bool
isDone s =
    case s of
        Done _ _ ->
            True

        _ ->
            False


isWon : BoardState -> Bool
isWon s =
    case s of
        Done Completed _ ->
            True

        _ ->
            False


doneState : BoardState -> ( Bool, Bool, Bool )
doneState state =
    case state of
        Done s r ->
            ( s == Completed, s == GameOver, r == Revealed )

        _ ->
            ( False, False, False )


type TimerEvent
    = Start
    | Stop
