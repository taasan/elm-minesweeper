module Types exposing
    ( Actor(..)
    ,  BoardEntry
       -- Prevent language server from converting list to one line

    , BoardState(..)
    , Cell(..)
    , CellMsg(..)
    , CellState
    , ChangeMethod(..)
    , Coordinate
    , DoneState(..)
    , ErrorMsg(..)
    , Flag(..)
    , GameMsg(..)
    , GridType(..)
    , IncomingCellsMsg
    , IncomingMsg(..)
    , Key(..)
    , Level
    , Mine(..)
    , Msg(..)
    , PlayState(..)
    , Revealed(..)
    , TimerEvent(..)
    , Topology(..)
    , boardStateEncoder
    , doneState
    , getIndex
    , inProgress
    , incomingMsgDecoder
    , paused
    )

import Browser.Events
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Page exposing (LevelStep, Modal)
import Random exposing (Seed)
import Set exposing (Set)
import Time
import Toasty
import Toasty.Defaults



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
    | GotActor Actor
    | Relax
    | VisibilityChanged Browser.Events.Visibility
    | GotModal Modal
    | PopModal
    | KeyPressed Key
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | GotIncomingMsg (Result D.Error IncomingMsg)
    | GotLevelChooserStep LevelStep


type alias IncomingCellsMsg =
    { poke : List Int, flag : List Int, state : Maybe DoneState }


type ErrorMsg
    = SolverError
    | UnknownError String


type IncomingMsg
    = IncomingCells IncomingCellsMsg
    | SolverStarted
    | Solution IncomingCellsMsg
    | Error ErrorMsg String


incomingMsgDecoder : D.Decoder IncomingMsg
incomingMsgDecoder =
    let
        incomingCellsDecoder : D.Decoder IncomingCellsMsg
        incomingCellsDecoder =
            D.succeed IncomingCellsMsg
                |> D.requiredAt [ "payload", "poke" ] (D.list D.int)
                |> D.requiredAt [ "payload", "flag" ] (D.list D.int)
                |> D.optionalAt [ "payload", "state" ] (D.maybe doneStateDecoder) Nothing

        payloadDecoder mapper decoder =
            mapper (D.required "payload" |> always decoder)

        errorDecoder : D.Decoder IncomingMsg
        errorDecoder =
            D.field "tag" D.string
                |> D.andThen
                    (\tag ->
                        let
                            eTag =
                                case tag of
                                    "Solver" ->
                                        SolverError

                                    _ ->
                                        UnknownError tag
                        in
                        D.succeed (Error eTag)
                            |> D.required "error" D.string
                    )

        okDecoder =
            D.field "tag" D.string
                |> D.andThen
                    (\tag ->
                        case tag of
                            "IncomingCells" ->
                                D.map IncomingCells incomingCellsDecoder

                            "SolverStarted" ->
                                D.succeed SolverStarted

                            "Solution" ->
                                payloadDecoder (D.map Solution) incomingCellsDecoder

                            _ ->
                                D.fail <| "Invalid tag " ++ tag
                    )
    in
    D.oneOf [ errorDecoder, okDecoder ]


type Key
    = Escape
    | Other


type Actor
    = Human
    | Robot


type GameMsg
    = NewGame
    | RandomGame


type CellMsg
    = GotFlagged (Set Int)
    | GotUnflagged (Set Int)
    | GotPoked (Set Int)



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


boardStateEncoder : BoardState -> E.Value
boardStateEncoder state =
    case state of
        NotInitialized ->
            E.string "NotInitialized"

        Initialized ->
            E.string "Initialized"

        Playing InProgress ->
            E.string "Playing InProgress"

        Playing (Paused _) ->
            E.string "Playing Paused"

        Done GameOver ->
            E.string "Done GameOver"

        Done Completed ->
            E.string "Done Completed"

        Demo ->
            E.string "Demo"


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
    , covered : a
    , revealed : a
    , uncovered : a
    }


type Revealed
    = Uncovered Int
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
    = Covered Int (Maybe Mine)
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
        Covered i _ ->
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


doneStateDecoder : D.Decoder DoneState
doneStateDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "GameOver" ->
                        D.succeed GameOver

                    "Completed" ->
                        D.succeed Completed

                    _ ->
                        D.fail <| "Unknown DoneState " ++ s
            )
