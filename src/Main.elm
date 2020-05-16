module Main exposing (main, update, view)

import Board exposing (Board, countStates)
import Browser
import Cell
import Grid exposing (GridType(..), Topology(..))
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Lib exposing (random)
import Random exposing (Seed)
import Symbol
import Task exposing (Task)
import Time exposing (Posix)
import Types exposing (BoardState(..), CellMsg(..), Msg(..), TimerEvent(..))
import View.Svg



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


solarized : String
solarized =
    "Solarized"



-- main =
--     Browser.element
--         { init = init
--         , update = update
--         , view = view
--         , subscriptions = subscriptions
--         }
--   game: {
--    board: GameRecord;
--    nextState: NextStateFunction;
--  };
--  loading: boolean;
--  containerRef: React.RefObject<SVGSVGElement>;
--  maxBoardDimensions: { maxWidth: string; maxHeight: string };
--  modalStack: ModalType[];
--  timingEvents: TimingEvent[];
--  elapsedTime(): number;
--  showMenu: boolean;
--  lives: 0 | 1 | 2;
--  rotated: boolean;
--  boardVersion: number;


type alias Model =
    { game : Board
    , seed : Seed
    , theme : String
    , currentTime : Time.Posix
    , timerEvents : List ( TimerEvent, Time.Posix )
    , error : Maybe String
    , debug : Cell.State Int
    }


mkModel : Time.Posix -> Maybe Seed -> Model
mkModel p x =
    let
        board =
            Board.emptyBoard

        level =
            Grid.beginner Hex Toroid

        seed =
            Maybe.withDefault (Random.initialSeed 37) x
    in
    { game =
        Board.mkBoard
            { board
                | level = level
                , seed = seed
                , useUncertainFlag = True
            }
    , seed = Random.initialSeed 37
    , theme = solarized
    , currentTime = p
    , timerEvents = []
    , error = Nothing
    , debug = Cell.map Lib.bool2int <| Cell.cellState Cell.Void
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        posixToMsg : Posix -> Msg
        posixToMsg p =
            p
                |> Time.posixToMillis
                |> Random.initialSeed
                |> GotSeed

        saved =
            case D.decodeValue decoder flags of
                Ok m ->
                    m

                Err err ->
                    { theme = solarized
                    , themes = [ solarized ]
                    , currentTime = Time.millisToPosix 0
                    , error = Just <| D.errorToString err
                    }

        model_ =
            mkModel saved.currentTime Nothing

        model =
            { model_ | theme = saved.theme, error = saved.error }
    in
    ( model, Task.perform posixToMsg Time.now )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ret model

        ret newModel =
            ( newModel, Cmd.none )

        ( gameState, board ) =
            model.game
    in
    case msg of
        TogglePause ->
            let
                ( b, s ) =
                    Board.togglePause model.game
            in
            case s of
                Just event ->
                    ( { model
                        | game = b
                      }
                    , Task.perform (GotTimerEvent event) Time.now
                    )

                _ ->
                    ret model

        NewGame seed ->
            if gameState == Paused || gameState == Playing then
                ret model

            else
                ret <| mkModel model.currentTime <| Just seed

        Cell msg_ ->
            let
                ( game, c ) =
                    Board.update msg_ model.game

                updatedModel =
                    { model
                        | game = game
                        , debug = Board.countStates <| getBoard game
                    }

                cmd =
                    case c of
                        Just x ->
                            Task.perform (GotTimerEvent x) Time.now

                        _ ->
                            Cmd.none
            in
            ( updatedModel, cmd )

        RandomGame ->
            ( model, Random.generate NewGame Random.independentSeed )

        GotSeed seed ->
            let
                ( _, s ) =
                    Random.step random seed
            in
            ( { model | game = ( gameState, { board | seed = s } ) }, Cmd.none )

        Relax ->
            noop

        SetTheme theme ->
            ret { model | theme = theme }

        GotTimerEvent event time ->
            ( { model | timerEvents = ( event, time ) :: model.timerEvents }, Cmd.none )

        GotCurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )


getBoard : Board -> Board.BoardRecord
getBoard b =
    let
        ( _, board ) =
            b
    in
    board


statusBar : Model -> Html Msg
statusBar model =
    let
        ( state, board ) =
            model.game

        { lives } =
            board.level

        stats =
            countStates board

        ( boardState, renderState ) =
            case state of
                NotInitialized ->
                    let
                        s =
                            NotInitialized
                    in
                    ( s, Symbol.toString <| Symbol.Board s )

                _ ->
                    ( state
                    , if state == Playing || Types.isDone state then
                        let
                            f n s =
                                String.repeat n <| Symbol.toString s
                        in
                        if Types.isWon state || state == Playing then
                            String.append
                                (f stats.exploded Symbol.ExplodedMine)
                                (f (lives - stats.exploded) (Symbol.Disarmed Symbol.Flagged))

                        else
                            f lives <| Symbol.Board state

                      else
                        Symbol.toString <| Symbol.Board state
                    )
    in
    div [ class "SvgMinesweeper__Controls" ]
        [ span [] [ text "Timer" ]
        , span
            [ onClick TogglePause ]
            [ span
                [ attribute "role" "img"
                , attribute "aria-label" "State"
                ]
                [ text renderState ]
            ]
        , span
            [ onClick RandomGame ]
            [ text (Board.boardState2String boardState) ]
        , span
            [ onClick <| NewGame model.seed ]
            [ span
                [ attribute "role" "img"
                , attribute "aria-label" "menu"
                ]
                [ text <| Symbol.toString Symbol.Hamburger ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ div [ class "SvgMinesweeper SvgMinesweeper__Container" ]
            [ statusBar model
            , Board.view
                { doPoke = \x -> Cell (GotPoked x)
                , doFlag = \x -> Cell (GotFlagged x)
                }
                model.game
            ]
        , View.Svg.defs
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PORTS
--port saveValue : E.Value -> Cmd msg
--
--
--
-- JSON ENCODE/DECODE


type alias SavedModel =
    { themes : List String
    , theme : String
    , currentTime : Time.Posix
    , error : Maybe String
    }


encode : SavedModel -> E.Value
encode model =
    E.object
        [ ( "themes", E.list E.string model.themes )
        , ( "theme", E.string model.theme )
        ]


type alias SavedModelL =
    { theme : String
    , themes : List String
    , currentTime : Time.Posix
    , error : Maybe String
    }


decoder : D.Decoder SavedModelL
decoder =
    D.map4 SavedModelL
        (D.field "theme" D.string)
        (D.field "themes" (D.list D.string))
        (D.field "currentTime" (D.map Time.millisToPosix D.int))
        (D.field "error" (D.succeed Nothing))
