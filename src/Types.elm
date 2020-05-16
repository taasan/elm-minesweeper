module Types exposing (BoardState(..), CellMsg(..), DoneState(..), Flag(..), Msg(..), RevealedState(..), TimerEvent(..), isDone, isWon)

import Random exposing (Seed)
import Time


type Msg
    = Cell CellMsg
    | TogglePause
    | NewGame Seed
    | SetTheme String
    | RandomGame
    | GotSeed Seed
    | GotCurrentTime Time.Posix
    | GotTimerEvent TimerEvent Time.Posix
    | Relax


type CellMsg
    = GotFlagged Int
    | GotPoked Int


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


type Flag
    = Flagged
    | Special
    | Incorrect
    | Uncertain


type TimerEvent
    = Begin
    | Play
    | Pause
    | End
