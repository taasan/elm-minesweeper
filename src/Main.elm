port module Main exposing (Model, NonEmpty, main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html
    exposing
        ( Html
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
import LevelChooser
import Lib exposing (getSeed)
import Minesweeper
    exposing
        ( Minesweeper
        , defaultHandlers
        , getBoard
        , mkBoard
        )
import Page exposing (Page)
import Random exposing (Seed)
import SvgHelper
import Symbol
import Task
import Time exposing (Posix)
import Types
    exposing
        ( BoardState(..)
        , Cell
        , CellMsg(..)
        , DoneState(..)
        , Flag(..)
        , GameMsg(..)
        , GridType(..)
        , Key(..)
        , Level
        , Msg(..)
        , StackOperation(..)
        , TimerEvent(..)
        , Topology(..)
        )
import Url exposing (Url)



-- MODEL


type alias Model =
    { currentTime : Time.Posix
    , elapsedTime : ( Int, Maybe Time.Posix )
    , error : Maybe String
    , board : Minesweeper
    , url : Url
    , key : Nav.Key
    , nextLevel : Level
    , seed : Seed
    , pages : NonEmpty Page
    }


type alias NonEmpty a =
    { head : a, tail : List a }


push : a -> NonEmpty a -> NonEmpty a
push x stack =
    if x == stack.head then
        stack

    else
        { head = x, tail = stack.head :: stack.tail }


pop : NonEmpty a -> ( a, NonEmpty a )
pop stack =
    case stack.tail of
        [] ->
            ( stack.head, stack )

        x :: xs ->
            ( stack.head, { head = x, tail = xs } )


pop_ : NonEmpty a -> NonEmpty a
pop_ =
    Tuple.second << pop



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


mkModel : Url -> Nav.Key -> Level -> Model
mkModel url key level =
    { currentTime = Time.millisToPosix 0
    , elapsedTime = ( 0, Nothing )
    , error = Nothing
    , board =
        mkBoard
            { lives = 3
            , useUncertainFlag = True
            , level = level
            }
    , url = url
    , key = key
    , nextLevel = level
    , seed = Random.initialSeed 0
    , pages = NonEmpty Page.Game []
    }


init : E.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        saved =
            case D.decodeValue decoder flags of
                Ok m ->
                    m

                Err _ ->
                    { theme = solarized
                    , themes = [ solarized, defaultTheme ]
                    , level = Minesweeper.beginner Square Plane
                    }

        model =
            mkModel url key saved.level
    in
    ( model
    , Cmd.batch
        [ Task.perform GotCurrentTime Time.now
        , Task.perform GotSeed getSeed
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (postUpdate model << update_ msg) model


handleGameMessage : GameMsg -> Model -> ( Model, Cmd Msg )
handleGameMessage msg model =
    let
        rec =
            getBoard model.board
    in
    case msg of
        NewGame level ->
            update (GotPage Replace Page.Game)
                { model
                    | board = mkBoard { rec | level = level }
                    , elapsedTime = ( 0, Nothing )
                }

        RandomGame level ->
            handleGameMessage (NewGame level)
                { model
                    | board = mkBoard rec
                    , seed = Random.step Random.independentSeed model.seed |> Tuple.first
                    , nextLevel = level
                }


handleCellMessage : CellMsg -> Model -> ( Model, Cmd Msg )
handleCellMessage msg model =
    let
        ( updatedBoard, event ) =
            Minesweeper.update model.seed msg model.board

        cmd =
            case event of
                Just x ->
                    Task.perform (GotTimerEvent x) Time.now

                _ ->
                    Cmd.none
    in
    ( { model | board = updatedBoard }, cmd )


doTogglePause : Model -> ( Model, Cmd Msg )
doTogglePause model =
    let
        ( b, s ) =
            Minesweeper.togglePause model.board
    in
    case s of
        Just event ->
            ( { model | board = b }, Task.perform (GotTimerEvent event) Time.now )

        _ ->
            ( model, Cmd.none )


postUpdate : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
postUpdate old ( new, cmd ) =
    if old.pages == new.pages || old == new then
        ( new, cmd )

    else
        let
            { pages } =
                new

            { state } =
                getBoard new.board

            shouldTogglePause =
                (pages.head == Page.Game && state == Paused)
                    || (pages.head /= Page.Game && state == Playing)
        in
        if shouldTogglePause then
            doTogglePause new
                |> Tuple.mapSecond (\x -> Cmd.batch [ x, cmd ])

        else
            ( new, cmd )


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
            doTogglePause model

        GotBlurred ->
            if state == Playing then
                doTogglePause model

            else
                noop

        Game msg_ ->
            handleGameMessage msg_ model

        Cell msg_ ->
            handleCellMessage msg_ model

        GotSeed seed ->
            ret { model | seed = seed }

        Relax ->
            noop

        GotTimerEvent event time ->
            let
                x =
                    if event == Start then
                        Just time

                    else
                        Nothing
            in
            ret { model | elapsedTime = ( elapsed time model.elapsedTime, x ) }

        GotCurrentTime time ->
            ret { model | currentTime = time }

        VisibilityChanged Browser.Events.Hidden ->
            update GotBlurred model

        VisibilityChanged Browser.Events.Visible ->
            noop

        GotLevel level ->
            let
                m =
                    { model | nextLevel = level, pages = model.pages }
            in
            ( m, saveLevel level )

        GotPage op page ->
            if model.pages.head == page then
                ret model

            else
                let
                    pages =
                        case op of
                            Replace ->
                                NonEmpty page model.pages.tail

                            Push ->
                                push page model.pages
                in
                ret { model | pages = pages }

        PopPage ->
            let
                pages =
                    pop_ model.pages
            in
            if pages == model.pages then
                noop

            else
                ret { model | pages = pop_ model.pages }

        KeyPressed Escape ->
            update PopPage model

        KeyPressed _ ->
            noop


statusBar : Model -> Html Msg
statusBar model =
    let
        { board } =
            model

        rec =
            getBoard board

        { state, lives, level } =
            rec

        { flagged, exploded } =
            rec.stats

        renderState =
            case state of
                Playing ->
                    let
                        f n s =
                            String.repeat n (Symbol.toString s)

                        { stats } =
                            rec
                    in
                    if lives > 0 then
                        f stats.exploded Symbol.ExplodedMine
                            ++ f (lives - stats.exploded) Symbol.Heart

                    else
                        Symbol.toString
                            (if state == Playing then
                                Symbol.Heart

                             else
                                Symbol.ExplodedMine
                            )

                _ ->
                    Symbol.toString (Symbol.Board state)

        elapsedTime =
            elapsed model.currentTime model.elapsedTime // 1000

        minutes =
            elapsedTime // 60

        seconds =
            modBy 60 elapsedTime

        zero : Char
        zero =
            Symbol.Count 0
                |> Symbol.toString
                >> String.uncons
                >> Maybe.withDefault ( '0', "" )
                >> Tuple.first

        zeroPad : Int -> String
        zeroPad =
            Symbol.Count >> Symbol.toString >> String.padLeft 2 zero

        timer : Html msg
        timer =
            [ minutes, seconds ]
                |> List.map zeroPad
                >> String.join ":"
                >> text

        itemClass =
            "SvgMinesweeper__Controls__Item"
    in
    div [ class "SvgMinesweeper__Controls" ]
        [ span
            [ title (String.fromInt elapsedTime), class "Timer" ]
            [ timer ]
        , span
            [ onClick
                (case state of
                    Done _ ->
                        Game (RandomGame level)

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
            [ onClick <| Game <| RandomGame level, class itemClass ]
            [ span
                [ class "FormatNumber" ]
                [ text (Symbol.toString (Symbol.Count (rec.stats.mined - flagged - exploded))) ]
            , span
                [ attribute "role" "img" ]
                [ text (Symbol.toString (Symbol.Flag Normal)) ]
            ]
        , div
            [ onClick (GotPage Push Page.LevelChooser), class itemClass ]
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
            case model.pages.head of
                Page.LevelChooser ->
                    ( Nothing
                    , div
                        []
                        [ lazy LevelChooser.view model.nextLevel ]
                        |> Html.map (Maybe.withDefault Relax)
                    )

                Page.Game ->
                    ( Just (lazy statusBar model)
                    , lazy2 Minesweeper.view defaultHandlers model.board
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
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        rec =
            getBoard model.board

        timer_ =
            case rec.state of
                Playing ->
                    Time.every 1000 GotCurrentTime

                _ ->
                    Sub.none
    in
    Sub.batch
        [ timer_
        , Browser.Events.onVisibilityChange VisibilityChanged
        , Browser.Events.onKeyDown (D.map KeyPressed keyDecoder)
        , windowBlurred (always GotBlurred)
        ]


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


port saveValue : E.Value -> Cmd msg


type alias SaveValueMsg =
    { key : String
    , value : E.Value
    }


saveLevel : Level -> Cmd msg
saveLevel level =
    saveValue <| saveValueEncoder { key = "level", value = levelEncoder level }



--
--
--
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
    }


decoder : D.Decoder Flags
decoder =
    D.succeed Flags
        |> D.optional "themes" (D.list D.string) [ solarized ]
        |> D.optional "theme" D.string solarized
        |> D.optional "level" levelDecoder (Minesweeper.beginner Square Plane)


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
        |> D.required "useUncertainFlag" D.bool


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
        , ( "useUncertainFlag", E.bool level.useUncertainFlag )
        ]


elapsed : Posix -> ( Int, Maybe Time.Posix ) -> Int
elapsed curr ( acc, e ) =
    case e of
        Just time ->
            acc + (Time.posixToMillis curr - Time.posixToMillis time)

        _ ->
            acc
