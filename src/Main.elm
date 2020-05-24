module Main exposing (main)

import Array
import Browser
import Html
    exposing
        ( Html
        , div
        , span
        , text
        )
import Html.Attributes exposing (attribute, class, title)
import Html.Events exposing (onClick)
import Html.Lazy as Html
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Lib exposing (bool2int)
import Minesweeper
    exposing
        ( Minesweeper
        , emptyCellState
        , getBoardRecord
        , mapCellState
        , mkBoard
        )
import Random
import SvgHelper
import Symbol
import Task
import Time exposing (Posix)
import Types
    exposing
        ( BoardState(..)
        , Cell(..)
        , CellMsg(..)
        , DoneState(..)
        , Flag(..)
        , GridType(..)
        , Level
        , Msg(..)
        , TimerEvent(..)
        , Topology(..)
        )



-- MODEL
-- MODEL


type alias Model =
    { theme : String
    , themes : List String
    , currentTime : Time.Posix
    , elapsedTime : ( Int, Maybe Time.Posix )
    , error : Maybe String
    , board : Minesweeper
    }



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


defaultTheme : String
defaultTheme =
    "Default"


mkModel : Level -> Model
mkModel level =
    { theme = solarized
    , themes = [ solarized, defaultTheme ]
    , currentTime = Time.millisToPosix 0
    , elapsedTime = ( 0, Nothing )
    , error = Nothing
    , board =
        mkBoard
            { cells = Array.empty
            , seed = Random.initialSeed 37
            , state = NotInitialized
            , lives = 3
            , useUncertainFlag = True
            , stats = mapCellState bool2int emptyCellState
            , level = level
            }
    }


getSeed : Posix -> Msg
getSeed p =
    p
        |> Time.posixToMillis
        |> Random.initialSeed
        |> GotSeed


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        saved =
            case D.decodeValue decoder flags of
                Ok m ->
                    m

                Err _ ->
                    { theme = solarized, themes = [ solarized, defaultTheme ] }

        model_ =
            mkModel <| Minesweeper.beginner Hex Toroid

        model =
            { model_ | theme = saved.theme, themes = saved.themes }
    in
    ( model
    , Cmd.batch
        [ Task.perform getSeed Time.now
        , Task.perform GotCurrentTime Time.now
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ret model

        ret newModel =
            ( newModel, Cmd.none )

        { board } =
            model

        rec =
            getBoardRecord board

        { state } =
            rec
    in
    case msg of
        TogglePause ->
            let
                ( b, s ) =
                    Minesweeper.togglePause board
            in
            case s of
                Just event ->
                    ( { model | board = b }, Task.perform (GotTimerEvent event) Time.now )

                _ ->
                    ret model

        NewGame ->
            if state == Paused || state == Playing then
                ret model

            else
                ret <|
                    { model
                        | board = mkBoard { rec | state = NotInitialized }
                        , elapsedTime = ( 0, Nothing )
                    }

        Cell msg_ ->
            let
                ( updatedBoard, event ) =
                    Minesweeper.update msg_ model.board

                cmd =
                    case event of
                        Just x ->
                            Task.perform (GotTimerEvent x) Time.now

                        _ ->
                            Cmd.none
            in
            ( { model | board = updatedBoard }, cmd )

        RandomGame ->
            let
                seed =
                    rec.seed
                        |> Random.step Random.independentSeed
                        |> Tuple.first
            in
            update NewGame { model | board = mkBoard { rec | seed = seed } }

        GotSeed seed ->
            ( { model | board = mkBoard { rec | state = state, seed = seed } }, Cmd.none )

        Relax ->
            noop

        SetTheme theme ->
            ret { model | theme = theme }

        GotTimerEvent event time ->
            let
                newTime =
                    elapsed time model.elapsedTime

                x =
                    if event == Start then
                        Just time

                    else
                        Nothing
            in
            ret { model | elapsedTime = ( newTime, x ), currentTime = time }

        GotCurrentTime time ->
            ret { model | currentTime = time }


statusBar : Model -> Html Msg
statusBar model =
    let
        { board } =
            model

        rec =
            getBoardRecord board

        { state, lives } =
            rec

        stateSymbol _ =
            Symbol.toString <| Symbol.Board state

        renderState =
            case state of
                NotInitialized ->
                    stateSymbol ()

                Done GameOver _ ->
                    stateSymbol ()

                _ ->
                    -- TODO simplify
                    if state == Playing || Types.isDone state then
                        let
                            f n s =
                                String.repeat n <| Symbol.toString s
                        in
                        if Types.isWon state || state == Playing then
                            let
                                { stats } =
                                    rec
                            in
                            if lives > 0 then
                                f stats.exploded Symbol.ExplodedMine
                                    ++ f (lives - stats.exploded) (Symbol.Disarmed Normal)

                            else
                                Symbol.toString
                                    (if state == Playing then
                                        Symbol.Disarmed Normal

                                     else
                                        Symbol.ExplodedMine
                                    )

                        else
                            stateSymbol ()

                    else
                        stateSymbol ()

        elapsedTime =
            elapsed model.currentTime model.elapsedTime // 1000

        timer : Html msg
        timer =
            elapsedTime
                |> Symbol.Count
                |> Symbol.toString
                |> text
    in
    div [ class "SvgMinesweeper__Controls" ]
        [ span [ title (String.fromInt elapsedTime) ] [ timer ]
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
            [ text (Minesweeper.boardState2String state) ]
        , span
            [ onClick <| NewGame ]
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
            , Html.lazy Minesweeper.view model.board
            ]
        , SvgHelper.defs
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        rec =
            getBoardRecord model.board
    in
    case rec.state of
        Playing ->
            Time.every 1000 GotCurrentTime

        _ ->
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
    }


encode : SavedModel -> E.Value
encode model =
    E.object
        [ ( "themes", E.list E.string model.themes )
        , ( "theme", E.string model.theme )
        ]


decoder : D.Decoder SavedModel
decoder =
    D.succeed SavedModel
        |> D.optional "themes" (D.list D.string) [ solarized ]
        |> D.optional "theme" D.string solarized


elapsed : Posix -> ( Int, Maybe Time.Posix ) -> Int
elapsed curr ( acc, e ) =
    case e of
        Just time ->
            acc + (Time.posixToMillis curr - Time.posixToMillis time)

        _ ->
            acc
