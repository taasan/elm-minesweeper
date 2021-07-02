module Types exposing
    ( BoardEntry
    , BoardState(..)
    , Cell(..)
    , CellMsg(..)
    , CellState
    , ChangeMethod(..)
    , Coordinate
    , DoneState(..)
    , Flag(..)
    , GameMsg(..)
    , GridType(..)
    , Key(..)
    , Level
    , Mine(..)
    , Msg(..)
    , PlayState(..)
    , Revealed(..)
    , StackOperation(..)
    , TimerEvent(..)
    , Topology(..)
    , doneState
    , getIndex
    , inProgress
    , paused
    )

import Browser.Events
import Page exposing (Page)
import Random exposing (Seed)
import Time



-- MESSAGE


type Msg
    = Cell CellMsg
    | Game GameMsg
    | TogglePause
    | GotBlurred
    | GotSeed Seed
    | GotCurrentTime Time.Posix
    | GotTimerEvent TimerEvent Time.Posix
    | GotLevel Level
    | Relax
    | VisibilityChanged Browser.Events.Visibility
    | GotPage StackOperation Page
    | PopPage
    | KeyPressed Key


type Key
    = Escape
    | Other


type StackOperation
    = Replace
    | Push


type GameMsg
    = NewGame Level
    | RandomGame Level


type CellMsg
    = GotFlagged Int
    | GotPoked Int



-- BOARD


type DoneState
    = Completed
    | GameOver


type ChangeMethod
    = Manual
    | Automatic


type PlayState
    = InProgress
    | Paused ChangeMethod


type BoardState
    = NotInitialized
    | Initialized
    | Playing PlayState
    | Done DoneState
    | Demo


inProgress : BoardState -> Bool
inProgress state =
    case state of
        Playing InProgress ->
            True

        _ ->
            False


paused : BoardState -> Bool
paused state =
    case state of
        Playing (Paused _) ->
            True

        _ ->
            False



-- LEVEL


type GridType
    = Hex
    | Square


type alias Level =
    { cols : Int
    , rows : Int
    , topology : Topology
    , type_ : GridType
    , mines : Float
    , useUncertainFlag : Bool
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


type alias BoardEntry =
    { cell : Cell
    , threats : Int
    , neighbours : List Int
    }


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
{- }
   type Shade
       = Dark
       | Light


   type alias Theme =
       { name : String
       , classes : List String
       , variant : Shade
       }
-}


doneState : BoardState -> ( Bool, Bool )
doneState state =
    case state of
        Done s ->
            ( s == Completed, s == GameOver )

        _ ->
            ( False, False )


type TimerEvent
    = Start
    | Stop
