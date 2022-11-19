module Player exposing
    (  Handler
       --

    , Handlers
    , Player(..)
    , PlayerInfo
    , Timer
    , elapsed
    , fromActor
    , info
    , startTimer
    , stopTimer
    )

import Html exposing (Attribute)
import Html.Events.Extra.Mouse as Event
import Set
import Symbol exposing (Symbol)
import Time exposing (Posix)
import Types
    exposing
        ( Actor(..)
          --
        , Cell(..)
        , CellMsg(..)
        , Speed
        , getIndex
        )


human : Player (Maybe CellMsg)
human =
    Player
        { actor = Human
        , flagBlockPoke = True
        , autoRevealSafe = True
        , revealNeighboursOnPoke = True
        , lives = 1
        , handlers = defaultHandlers
        , symbol = Symbol.Heart
        , revealAllOnGameOver = True
        , elapsedTime = Just { acc = 0, lastStart = Nothing }
        }


robot : Speed -> Player Never
robot speed =
    Player
        { actor = Robot speed
        , flagBlockPoke = False
        , autoRevealSafe = False
        , revealNeighboursOnPoke = False
        , lives = 1
        , handlers = Nothing
        , symbol = Symbol.Machine
        , revealAllOnGameOver = False
        , elapsedTime = Nothing
        }


type alias PlayerInfo msg =
    { actor : Actor
    , flagBlockPoke : Bool
    , autoRevealSafe : Bool
    , revealNeighboursOnPoke : Bool
    , lives : Int
    , handlers : Maybe (Handlers msg)
    , symbol : Symbol
    , revealAllOnGameOver : Bool
    , elapsedTime : Maybe { acc : Int, lastStart : Maybe Posix }
    }


type alias Timer =
    { acc : Int, lastStart : Maybe Posix }


stopTimer : Posix -> Player msg -> Player msg
stopTimer now ((Player player) as p) =
    case player.elapsedTime of
        Just timer ->
            case timer.lastStart of
                Nothing ->
                    p

                _ ->
                    Player { player | elapsedTime = Just { timer | acc = elapsed now timer, lastStart = Nothing } }

        Nothing ->
            p


startTimer : Posix -> Player msg -> Player msg
startTimer now ((Player player) as p) =
    case player.elapsedTime of
        Just timer ->
            case timer.lastStart of
                Just _ ->
                    p

                _ ->
                    Player { player | elapsedTime = Just { timer | acc = elapsed now timer, lastStart = Just now } }

        Nothing ->
            p


elapsed : Posix -> Timer -> Int
elapsed now { acc, lastStart } =
    case lastStart of
        Just time ->
            let
                diff =
                    Time.posixToMillis now - Time.posixToMillis time
            in
            if diff > 0 then
                acc + diff

            else
                acc

        _ ->
            acc


type Player msg
    = Player (PlayerInfo msg)


map : (Maybe (Handlers a) -> Maybe (Handlers b)) -> Player a -> Player b
map f (Player p) =
    Player
        { actor = p.actor
        , flagBlockPoke = p.flagBlockPoke
        , autoRevealSafe = p.autoRevealSafe
        , revealNeighboursOnPoke = p.revealNeighboursOnPoke
        , lives = p.lives
        , handlers = f p.handlers
        , symbol = p.symbol
        , revealAllOnGameOver = p.revealAllOnGameOver
        , elapsedTime = p.elapsedTime
        }


fromActor : Actor -> Player (Maybe CellMsg)
fromActor actor =
    case actor of
        Human ->
            human

        Robot speed ->
            map (always Nothing) (robot speed)


info : Player msg -> PlayerInfo msg
info (Player x) =
    x


type alias Handler msg =
    Cell -> Attribute msg


type alias Handlers msg =
    { poker : Handler msg
    , flagger : Handler msg
    }


stopEvent : String -> (Event.Event -> msg) -> Attribute msg
stopEvent e =
    { stopPropagation = True, preventDefault = True }
        |> Event.onWithOptions e


defaultPoker : Handler (Maybe CellMsg)
defaultPoker cell =
    stopEvent "mousedown"
        (\e ->
            case e.button of
                Event.MainButton ->
                    (Just << GotPoked << Set.singleton << getIndex) cell

                _ ->
                    Nothing
        )


defaultFlagger : Handler (Maybe CellMsg)
defaultFlagger cell =
    let
        msg i =
            case cell of
                Flagged _ _ _ ->
                    Just (GotUnflagged i)

                Covered _ _ ->
                    Just (GotFlagged i)

                _ ->
                    Nothing
    in
    stopEvent "contextmenu"
        (\_ -> (msg << Set.singleton << getIndex) cell)


defaultHandlers : Maybe (Handlers (Maybe CellMsg))
defaultHandlers =
    Just
        { poker = defaultPoker
        , flagger = defaultFlagger
        }
