port module Main exposing (Model, main)

import Array
import Basics.Extra exposing (flip)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html
    exposing
        ( Html
          --
        , div
        , header
        , main_
        , span
        , text
        )
import Html.Attributes exposing (attribute, class, title)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Lib exposing (getSeed)
import Minesweeper
    exposing
        ( Minesweeper
          --
        , cellState
        , getBoard
        , mkBoard
        , numMines
        , paused
        )
import Page exposing (LevelStep(..), Modal)
import Page.LevelChooser as LevelChooser
import Player exposing (Player)
import Random exposing (Seed)
import Set
import SvgHelper
import Symbol
import Task
import Time
import Toasty
import Toasty.Defaults
import Types
    exposing
        ( Actor(..)
        , BoardState(..)
        , CellMsg(..)
        , ChangeMethod(..)
        , DoneState(..)
        , ErrorMsg(..)
        , Flag(..)
        , GameMsg(..)
        , GridType(..)
        , IncomingMsg(..)
        , Key(..)
        , Level
        , Msg(..)
        , PlayState(..)
        , TimerEvent(..)
        , Topology(..)
        , boardStateEncoder
        , inProgress
        , incomingMsgDecoder
        )
import Url exposing (Url)



-- MODEL


type alias Model =
    { currentTime : Time.Posix
    , board : Minesweeper
    , url : Url
    , key : Nav.Key
    , nextLevel : ( Level, Actor )
    , seed : Seed
    , modals : List Modal
    , player : Player (Maybe CellMsg)
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    , solver : Maybe Int
    }



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = always Relax
        , onUrlRequest = always Relax
        }



-- PORTS


solarized : String
solarized =
    "Solarized"


defaultTheme : String
defaultTheme =
    "Default"


mkModel : Url -> Nav.Key -> Flags -> Model
mkModel url key { level, actor } =
    { currentTime = Time.millisToPosix 0
    , board = mkBoard { level = level }
    , url = url
    , key = key
    , nextLevel = ( level, actor )
    , seed = Random.initialSeed 0
    , modals = []
    , player = Player.fromActor actor
    , toasties = Toasty.initialState
    , solver = Nothing
    }


init : E.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( saved, error ) =
            case D.decodeValue decoder flags of
                Ok m ->
                    ( m, Nothing )

                Err err ->
                    ( { theme = solarized
                      , themes = [ solarized, defaultTheme ]
                      , level = Minesweeper.beginner Square Plane
                      , actor = Human
                      }
                    , (Just << D.errorToString) err
                    )

        model =
            mkModel url key saved

        toasty =
            case error of
                Just err ->
                    addPersistentToast <| Toasty.Defaults.Error "Unable to decode flags" err

                Nothing ->
                    identity
    in
    ( model
    , Cmd.batch
        [ Task.perform GotCurrentTime Time.now
        , Task.perform GotSeed getSeed
        ]
    )
        |> postUpdate model
        |> toasty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (postUpdate model << update_ msg) model


startMachineGame : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startMachineGame ( model, cmd ) =
    if model.solver /= Nothing then
        ( model, cmd )

    else
        let
            oldBoard =
                getBoard model.board

            newBoard =
                Minesweeper.initialize model.seed <| mkBoard oldBoard

            { level } =
                getBoard newBoard
        in
        ( { model | board = newBoard, solver = Just 0 }, cmd )
            |> (addCmd << sendOutgoingMessage << StartSolver) level


addCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
addCmd cmd ( a, cmd2 ) =
    ( a, Cmd.batch [ cmd, cmd2 ] )


append : (Model -> ( Model, Cmd msg )) -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
append f ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            f model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


handleGameMessage : GameMsg -> Model -> ( Model, Cmd Msg )
handleGameMessage msg model =
    let
        rec =
            getBoard model.board
    in
    case msg of
        NewGame ->
            let
                ( level, actor ) =
                    model.nextLevel

                f =
                    case actor of
                        Robot ->
                            startMachineGame

                        Human ->
                            identity
            in
            ( { model
                | board = Minesweeper.initialize model.seed <| mkBoard { rec | level = level }
                , player = Player.fromActor actor
                , modals = []
              }
            , Cmd.none
            )
                |> f

        RandomGame ->
            handleGameMessage NewGame
                { model | seed = Random.step Random.independentSeed model.seed |> Tuple.first }


handleCellMessage : CellMsg -> Model -> ( Model, Cmd Msg )
handleCellMessage msg model =
    if paused model.board then
        ( model, Cmd.none )

    else
        let
            board =
                model.board

            ( updatedBoard, event ) =
                Minesweeper.update model.player msg board

            timerCmd =
                case event of
                    Just x ->
                        Task.perform (GotTimerEvent x) Time.now

                    _ ->
                        Cmd.none

            newRec =
                getBoard updatedBoard

            cellUncoveredCmd =
                case model.solver of
                    Just _ ->
                        case msg of
                            GotPoked cells ->
                                let
                                    mapper : Int -> Maybe CellRevealedResponse
                                    mapper i =
                                        Array.get i newRec.entries
                                            |> Maybe.map
                                                (\e ->
                                                    let
                                                        { mined, flagged } =
                                                            cellState e.cell
                                                    in
                                                    { threats = e.threats
                                                    , neighbours = e.neighbours
                                                    , index = i
                                                    , mined = mined
                                                    , flagged = flagged
                                                    }
                                                )
                                in
                                cells
                                    |> (List.filterMap mapper << Set.toList)
                                    |> (sendOutgoingMessage << CellsUncovered)

                            _ ->
                                Cmd.none

                    _ ->
                        Cmd.none
        in
        ( { model | board = updatedBoard }, Cmd.batch [ timerCmd, cellUncoveredCmd ] )


doTogglePause : ChangeMethod -> Model -> ( Model, Cmd Msg )
doTogglePause pauseState model =
    if (.actor << Player.info << .player) model /= Human then
        ( model, Cmd.none )

    else
        let
            ( b, s ) =
                Minesweeper.togglePause pauseState model.board
        in
        case s of
            Just event ->
                ( { model | board = b }, Task.perform (GotTimerEvent event) Time.now )

            _ ->
                ( model, Cmd.none )


postUpdate : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
postUpdate old ( new, originalCmd ) =
    let
        state =
            .state << getBoard <| new.board

        oldState =
            .state << getBoard <| old.board

        level =
            .level << getBoard <| new.board

        value =
            [ ( "old", oldState ), ( "new", state ) ]
                |> List.map (Tuple.mapSecond boardStateEncoder)
                |> (::) ( "level", levelEncoder { level | mines = (toFloat << numMines) level } )
                |> E.object

        stateChangedCmd =
            if oldState /= state then
                stateChanged value

            else
                Cmd.none

        toastyCmd =
            if oldState /= state && state == Done Completed then
                case getTime new of
                    Just ( _, time ) ->
                        Toasty.Defaults.Success (Symbol.toString (Symbol.Board state)) time
                            |> addToast

                    _ ->
                        identity

            else
                identity

        cmd =
            Cmd.batch [ originalCmd, stateChangedCmd ]

        solver =
            case state of
                Playing InProgress ->
                    new.solver

                _ ->
                    Nothing
    in
    if old.modals == new.modals || old == new then
        ( new, cmd )
            |> toastyCmd

    else
        let
            { modals } =
                new

            shouldTogglePause =
                (state /= Playing (Paused Manual))
                    && (modals == [] && paused new.board)
                    || (modals /= [] && inProgress state)
        in
        if shouldTogglePause then
            doTogglePause Automatic new
                |> Tuple.mapSecond (\x -> Cmd.batch [ x, cmd ])

        else
            ( { new | solver = solver }, cmd )
                |> toastyCmd


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    let
        noop =
            ret model

        ret newModel =
            ( newModel, Cmd.none )

        { board } =
            model

        rec =
            getBoard board

        { state } =
            rec
    in
    case msg of
        TogglePause ->
            doTogglePause Manual model

        GotBlurred ->
            if inProgress state then
                doTogglePause Automatic model

            else
                noop

        Game msg_ ->
            handleGameMessage msg_ model

        Cell msg_ ->
            case state of
                NotInitialized ->
                    noop

                _ ->
                    handleCellMessage msg_ model

        GotSeed seed ->
            ret { model | seed = seed }

        Relax ->
            noop

        GotTimerEvent event time ->
            let
                f =
                    if event == Start then
                        Player.startTimer

                    else
                        Player.stopTimer
            in
            ret { model | player = f time model.player }

        GotCurrentTime time ->
            ret { model | currentTime = time }

        VisibilityChanged Browser.Events.Hidden ->
            --update GotBlurred model
            noop

        VisibilityChanged Browser.Events.Visible ->
            if state == Playing (Paused Automatic) then
                doTogglePause Automatic model

            else
                noop

        GotLevel level ->
            let
                m =
                    { model | nextLevel = Tuple.mapFirst (always level) model.nextLevel }
            in
            ( m, saveLevel level )

        GotModal modal ->
            case model.modals of
                [] ->
                    ret { model | modals = [ modal ] }

                x :: xs ->
                    if x == modal then
                        ret model

                    else
                        let
                            modals =
                                modal :: x :: xs
                        in
                        ret { model | modals = modals }

        PopModal ->
            case model.modals of
                [] ->
                    noop

                _ :: xs ->
                    ret { model | modals = xs }

        KeyPressed Escape ->
            update PopModal model

        KeyPressed _ ->
            noop

        GotActor actor ->
            ret { model | nextLevel = Tuple.mapSecond (always actor) model.nextLevel }
                |> addCmd (saveActor actor)

        ToastyMsg subMsg ->
            Toasty.update Toasty.Defaults.config ToastyMsg subMsg model

        GotIncomingMsg result ->
            case result of
                Ok (IncomingCells incoming) ->
                    let
                        doFlag =
                            incoming.flag
                                |> (handleCellMessage << GotFlagged << Set.fromList)

                        doPoke =
                            incoming.poke
                                |> (handleCellMessage << GotPoked << Set.fromList)

                        updated : ( Model, Cmd Msg )
                        updated =
                            model
                                |> doFlag
                                |> append doPoke
                    in
                    updated

                Ok SolverStarted ->
                    ret { model | solver = Just 0 }

                Ok (Solution s) ->
                    let
                        f get cellMsg =
                            case get s of
                                [] ->
                                    flip Tuple.pair Cmd.none

                                xs ->
                                    xs |> (handleCellMessage << cellMsg << Set.fromList)

                        updated : ( Model, Cmd Msg )
                        updated =
                            let
                                solver =
                                    case s.state of
                                        Just _ ->
                                            Nothing

                                        _ ->
                                            model.solver
                            in
                            { model | solver = solver }
                                |> f .flag GotFlagged
                                |> append (f .poke GotPoked)

                        cmd =
                            case .state << getBoard << .board << Tuple.first <| updated of
                                Playing InProgress ->
                                    sendOutgoingMessage GetStep

                                _ ->
                                    Cmd.none
                    in
                    updated
                        |> addCmd cmd

                Ok (Error SolverError error) ->
                    ( model, Cmd.none )
                        |> addPersistentToast (Toasty.Defaults.Error "Solver error" error)

                Ok (Error (UnknownError tag) error) ->
                    ( model, Cmd.none )
                        |> addPersistentToast (Toasty.Defaults.Error ("Unknown error (" ++ tag ++ ")") error)

                Err error ->
                    ( model, Cmd.none )
                        |> addPersistentToast (Toasty.Defaults.Error "Error decoding incoming message" <| D.errorToString error)

        GotLevelChooserStep step ->
            case step of
                Actor ->
                    ret
                        { model
                            | modals = Page.LevelChooser Topology :: model.modals
                        }

                Topology ->
                    ret
                        { model
                            | modals = Page.LevelChooser GridType :: model.modals
                        }

                GridType ->
                    ret
                        { model
                            | modals = Page.LevelChooser Dimensions :: model.modals
                        }

                Dimensions ->
                    update (Game RandomGame) { model | modals = [] }


getTime : Model -> Maybe ( Int, String )
getTime model =
    let
        player =
            Player.info model.player
    in
    case player.elapsedTime of
        Just et ->
            let
                elapsedTime =
                    Player.elapsed model.currentTime et // 1000

                minutes =
                    elapsedTime // 60

                seconds =
                    modBy 60 elapsedTime

                time =
                    [ minutes, seconds ]
                        |> List.map zeroPad
                        >> String.join ":"

                zeroPad : Int -> String
                zeroPad =
                    Symbol.Count >> Symbol.toString >> String.padLeft 2 zero

                zero : Char
                zero =
                    Symbol.Count 0
                        |> Symbol.toString
                        >> String.uncons
                        >> Maybe.withDefault ( '0', "" )
                        >> Tuple.first
            in
            Just ( elapsedTime, time )

        _ ->
            Nothing


statusBar : Model -> Html Msg
statusBar model =
    let
        { board } =
            model

        rec =
            getBoard board

        { state } =
            rec

        player =
            Player.info model.player

        { flagged, exploded } =
            rec.stats

        renderState =
            case state of
                Playing InProgress ->
                    let
                        f n s =
                            String.repeat n (Symbol.toString s)

                        { stats } =
                            rec
                    in
                    if player.lives > 0 then
                        f stats.exploded Symbol.ExplodedMine
                            ++ f (player.lives - stats.exploded) player.symbol

                    else
                        Symbol.toString
                            (if inProgress state then
                                player.symbol

                             else
                                Symbol.ExplodedMine
                            )

                _ ->
                    Symbol.toString (Symbol.Board state)

        firstSlot : Html msg
        firstSlot =
            case getTime model of
                Just ( elapsedTime, time ) ->
                    span
                        [ title (String.fromInt elapsedTime), class "Timer" ]
                        [ text time ]

                _ ->
                    (text << Symbol.toString) player.symbol

        itemClass =
            "SvgMinesweeper__Controls__Item"
    in
    div [ class "SvgMinesweeper__Controls" ]
        [ firstSlot
        , span
            [ onClick
                (case state of
                    Done _ ->
                        Game RandomGame

                    Initialized ->
                        Game RandomGame

                    NotInitialized ->
                        Game RandomGame

                    _ ->
                        TogglePause
                )
            ]
            [ span
                [ attribute "role" "img"
                , attribute "aria-label" "State"
                ]
                [ text renderState ]
            ]
        , div
            [ class itemClass ]
            [ span
                [ class "FormatNumber" ]
                [ text (Symbol.toString (Symbol.Count (rec.stats.mined - flagged - exploded))) ]
            , span
                [ attribute "role" "img" ]
                [ text (Symbol.toString (Symbol.Flag Normal)) ]
            ]
        , div
            [ onClick (GotModal (Page.LevelChooser Actor)), class itemClass ]
            [ span
                [ attribute "role" "img"
                , attribute "aria-label" "menu"
                ]
                [ text (Symbol.toString Symbol.Hamburger) ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        ( header_, content ) =
            case model.modals of
                x :: _ ->
                    case x of
                        Page.LevelChooser step ->
                            ( Nothing
                            , div
                                []
                                [ lazy2 LevelChooser.view model.nextLevel step ]
                                |> Html.map (Maybe.withDefault Relax)
                            )

                [] ->
                    ( Just (lazy statusBar model)
                    , lazy2 Minesweeper.view ((.handlers << Player.info) model.player) model.board
                        |> Html.map (Maybe.withDefault Relax << Maybe.map Cell)
                    )
    in
    { title = "Minesweeper"
    , body =
        [ div [ class "SvgMinesweeper SvgMinesweeper__Container" ]
            (List.filterMap identity
                [ Maybe.map (\x -> header [] [ x ]) header_
                , Just (main_ [] [ content ])
                ]
            )
        , SvgHelper.defs
        , Toasty.view Toasty.Defaults.config Toasty.Defaults.view ToastyMsg model.toasties
        ]
    }


addPersistentToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addPersistentToast toast ( model, cmd ) =
    Toasty.addPersistentToast Toasty.Defaults.config ToastyMsg toast ( model, cmd )


addToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Toasty.Defaults.config ToastyMsg toast ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        rec =
            getBoard model.board

        useClock =
            (.elapsedTime << Player.info) model.player /= Nothing

        timer_ =
            if useClock && rec.state == Playing InProgress then
                Time.every 1000 GotCurrentTime

            else
                Sub.none

        keyDown =
            case model.modals of
                [] ->
                    Sub.none

                _ ->
                    Browser.Events.onKeyDown (D.map KeyPressed keyDecoder)

        aiSubs =
            [ --poke (Cell << GotPoked << Set.fromList)
              --, flag (Cell << GotFlagged << Set.fromList)
              receive (GotIncomingMsg << D.decodeValue incomingMsgDecoder)
            ]

        solver =
            --case model.solver of
            --    Just _ ->
            --        Browser.Events.onAnimationFrame GotAnimationFrame
            --
            --    _ ->
            Sub.none
    in
    [ timer_
    , Browser.Events.onVisibilityChange VisibilityChanged
    , keyDown
    , windowBlurred (always GotBlurred)
    , solver
    ]
        ++ aiSubs
        |> Sub.batch


keyDecoder : D.Decoder Key
keyDecoder =
    D.map toDirection (D.field "key" D.string)


toDirection : String -> Key
toDirection string =
    case string of
        "Escape" ->
            Escape

        _ ->
            Other



-- PORTS


port windowBlurred : (String -> msg) -> Sub msg


type alias SaveValueMsg =
    { key : String
    , value : E.Value
    }


saveLevel : Level -> Cmd msg
saveLevel level =
    sendOutgoingMessage <| SaveValue { key = "level", value = levelEncoder level }


saveActor : Actor -> Cmd msg
saveActor actor =
    let
        value =
            case actor of
                Human ->
                    "Human"

                Robot ->
                    "Robot"
    in
    sendOutgoingMessage <| SaveValue { key = "actor", value = E.string value }



--port poke : (List Int -> msg) -> Sub msg
--
--
--port flag : (List Int -> msg) -> Sub msg


port stateChanged : E.Value -> Cmd msg


type alias CellRevealedResponse =
    { index : Int
    , threats : Int
    , mined : Bool
    , flagged : Bool
    , neighbours : List Int
    }


cellRevealedResponseEncoder : CellRevealedResponse -> E.Value
cellRevealedResponseEncoder res =
    E.object
        [ ( "index", E.int res.index )
        , ( "threats", E.int res.threats )
        , ( "mined", E.bool res.mined )
        , ( "flagged", E.bool res.flagged )
        , ( "neighbours", E.list E.int res.neighbours )
        ]


type OutgoingMsg
    = CellsUncovered (List CellRevealedResponse)
    | SaveValue SaveValueMsg
    | StartSolver Level
    | GetStep


outgoingMsgEncoder : OutgoingMsg -> E.Value
outgoingMsgEncoder msg =
    let
        encode : String -> Maybe E.Value -> E.Value
        encode tag payload =
            case payload of
                Just x ->
                    E.object
                        [ ( "tag", E.string tag )
                        , ( "payload", x )
                        ]

                _ ->
                    E.object [ ( "tag", E.string tag ) ]
    in
    case msg of
        CellsUncovered res ->
            Just (E.list cellRevealedResponseEncoder res)
                |> encode "CellsUncovered"

        StartSolver level ->
            Just (levelEncoder { level | mines = (toFloat << numMines) level })
                |> encode "StartSolver"

        SaveValue value ->
            Just (saveValueEncoder value)
                |> encode "SaveValue"

        GetStep ->
            encode "GetStep" Nothing


sendOutgoingMessage : OutgoingMsg -> Cmd msg
sendOutgoingMessage =
    send << outgoingMsgEncoder


port send : E.Value -> Cmd msg


port receive : (E.Value -> msg) -> Sub msg



-- JSON ENCODE/DECODE


saveValueEncoder : SaveValueMsg -> E.Value
saveValueEncoder msg =
    E.object
        [ ( "key", E.string msg.key )
        , ( "value", msg.value )
        ]


type alias Flags =
    { themes : List String
    , theme : String
    , level : Level
    , actor : Actor
    }


decoder : D.Decoder Flags
decoder =
    D.succeed Flags
        |> D.optional "themes" (D.list D.string) [ solarized ]
        |> D.optional "theme" D.string solarized
        |> D.optional "level" levelDecoder (Minesweeper.beginner Square Plane)
        |> D.optional "actor" actorDecoder Human


actorDecoder : D.Decoder Actor
actorDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Human" ->
                        D.succeed Human

                    "Robot" ->
                        D.succeed Robot

                    _ ->
                        D.fail <| "Invalid Topology " ++ s
            )


topologyDecoder : D.Decoder Topology
topologyDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Toroid" ->
                        D.succeed Toroid

                    "Plane" ->
                        D.succeed Plane

                    _ ->
                        D.fail <| "Invalid Topology " ++ s
            )


gridTypeDecoder : D.Decoder GridType
gridTypeDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Hex" ->
                        D.succeed Hex

                    "Square" ->
                        D.succeed Square

                    _ ->
                        D.fail <| "Invalid GridType " ++ s
            )


levelDecoder : D.Decoder Level
levelDecoder =
    D.succeed Level
        |> D.required "cols" D.int
        |> D.required "rows" D.int
        |> D.required "topology" topologyDecoder
        |> D.required "type" gridTypeDecoder
        |> D.required "mines" D.float


gridTypeEncoder : GridType -> E.Value
gridTypeEncoder x =
    case x of
        Hex ->
            E.string "Hex"

        Square ->
            E.string "Square"


topologyEncoder : Topology -> E.Value
topologyEncoder x =
    case x of
        Plane ->
            E.string "Plane"

        Toroid ->
            E.string "Toroid"


levelEncoder : Level -> E.Value
levelEncoder level =
    E.object
        [ ( "cols", E.int level.cols )
        , ( "rows", E.int level.rows )
        , ( "topology", topologyEncoder level.topology )
        , ( "type", gridTypeEncoder level.type_ )
        , ( "mines", E.float level.mines )
        ]
