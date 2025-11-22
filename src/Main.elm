port module Main exposing (main)

import Player exposing (Player)
import Util exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random


port alert : String -> Cmd msg
port savePlayers : Encode.Value -> Cmd msg
port clearLocalStorage : () -> Cmd msg


type alias Flags =
    Encode.Value


type alias Model =
    { newPlayerInput : String
    , teamCountInput : String
    , players : Array Player
    }


type Msg
    = ChangeNewPlayerInput String
    | ChangeTeamCountInput String
    | AddNewPlayer
    | AssignTeams
    | PlayersShuffled (Array Player)
    | ClearPlayers
    | RemovePlayer Int


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


initModel : Model
initModel =
    { newPlayerInput = ""
    , teamCountInput = defaultTeamCount
    , players = Array.empty
    }


savedLocalStorageDecoder : Decoder { players : Array Player }
savedLocalStorageDecoder =
    Decode.map (\ players -> { players = players } )
        (Decode.field "savedPlayers"
            (Decode.array Player.decoder))


init : Flags -> ( Model, Cmd Msg )
init savedLocalStorage =
    case Decode.decodeValue savedLocalStorageDecoder savedLocalStorage of
        Ok savedLocalStorageDecoded ->
            ( { initModel | players = savedLocalStorageDecoded.players }, Cmd.none )

        Err e ->
            ( initModel, Cmd.batch [clearLocalStorage (), alert (Decode.errorToString e)])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNewPlayerInput newValue ->
            ( { model | newPlayerInput = newValue }, Cmd.none )

        AddNewPlayer ->
            if not (String.isEmpty model.newPlayerInput) then
                updatePlayers (Array.push (Player.Unassigned model.newPlayerInput) model.players) { model | newPlayerInput = "" }
            else
                ( model, Cmd.none )

        ChangeTeamCountInput newValue ->
            ( { model | teamCountInput = newValue }, Cmd.none )

        AssignTeams ->
            ( model, Random.generate PlayersShuffled (shuffle model.players) )

        PlayersShuffled shuffled ->
            assignTeams shuffled model

        ClearPlayers ->
            updatePlayers Array.empty model

        RemovePlayer i ->
            updatePlayers (Array.append (Array.slice 0 i model.players) (Array.slice (i + 1) (Array.length model.players) model.players)) model


updatePlayers : Array Player -> Model -> ( Model, Cmd Msg )
updatePlayers newPlayers model =
    ( { model | players = newPlayers }, savePlayers (Encode.array Player.encoder newPlayers) )


defaultTeamCount : String
defaultTeamCount =
    "2"


teams : List String
teams =
    [ "red"
    , "blue"
    , "green"
    , "orange"
    , "purple"
    , "gold"
    , "brown"
    , "pink"
    ]


nAllTeams : String
nAllTeams =
    String.fromInt (List.length teams)


assignTeams : Array Player -> Model -> ( Model, Cmd Msg )
assignTeams shuffled model =
    case String.toInt model.teamCountInput of
        Nothing ->
            ( model, alert "Number of teams must be a string." )
                
        Just nTeams ->
            if nTeams < 2 || nTeams > List.length teams then
                ( model
                , alert
                ("Number of teams must be greater than or equal to "
                    ++ defaultTeamCount ++ " and less than or equal to "
                    ++ nAllTeams ++ ".")
                )
            else
            let
                nPlayers =
                    Array.length shuffled
            in
                if nTeams > nPlayers then
                    ( model
                    , alert "Number of teams must be less than or equal to the number of players."
                    )
                else
                    doAssignTeams nPlayers nTeams shuffled model


doAssignTeams : Int -> Int -> Array Player -> Model -> ( Model, Cmd Msg )
doAssignTeams nPlayers nTeams shuffled model =
    let
        idealTeamSize =
            nPlayers // nTeams
            
        nResiduals =
            nPlayers
            |> modBy nTeams

        assignments =
            -- Figure out each team size
            List.map2 Tuple.pair
                teams (List.repeat nResiduals (idealTeamSize + 1) ++ List.repeat (nPlayers - (nResiduals * (idealTeamSize + 1))) idealTeamSize)
            -- Expand the assignments
            |> List.concatMap (\ ( team, nAssignments ) ->
                List.repeat nAssignments team)

            -- Assign the players
            |> List.map2 (\ player team ->
                case player of
                    Player.Unassigned name ->
                        Player.Assigned name team

                    Player.Assigned name _ ->
                        Player.Assigned name team)

                (Array.toList shuffled)

            |> Array.fromList
    in
        updatePlayers assignments model


view : Model -> Html Msg
view model =
    main_ []
        [ Html.form [onSubmit AddNewPlayer]
            [ label []
                [ text "New Player Name"
                , input [value model.newPlayerInput, onInput ChangeNewPlayerInput] []
                ]
            , button [type_ "submit"] [text "Add"]
            , button [type_ "button", onClick ClearPlayers] [text "Clear"]
            ]
        , div [] (List.indexedMap playerView (Array.toList model.players))
        , Html.form [onSubmit AssignTeams]
            [ label []
                [ text "Number of Teams"
                , input
                [ value model.teamCountInput
                , onInput ChangeTeamCountInput
                , type_ "number"
                , Html.Attributes.min defaultTeamCount
                , Html.Attributes.max nAllTeams
                ]
                []
            ]
            , button [type_ "submit"] [text "Assign Teams!"]
            ]
        ]


onRightClick : msg -> Attribute msg
onRightClick msg =
    preventDefaultOn "contextmenu" (Decode.succeed ( msg, True ))


basePlayerView : Int -> String -> String -> Html Msg
basePlayerView i name team =
    div
        [ class "player"
        , class team
        , onRightClick (RemovePlayer i)
        ]
        [text name]


playerView : Int -> Player -> Html Msg
playerView i player =
    case player of
        Player.Unassigned name ->
            basePlayerView i name "unassigned"

        Player.Assigned name team ->
            basePlayerView i name team


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none