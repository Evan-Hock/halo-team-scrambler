port module Main exposing (main)

import Player exposing (Player)
import Util

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed as Keyed
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random


port alert : String -> Cmd msg
port savePlayers : Encode.Value -> Cmd msg
port clearLocalStorage : () -> Cmd msg


type alias Flags =
    ()


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
    | AssignTeams0 (Array Int)
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


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNewPlayerInput newValue ->
            ( { model | newPlayerInput = newValue }, Cmd.none )

        AddNewPlayer ->
            if not (String.isEmpty model.newPlayerInput) then
                let
                    newPlayer =
                        { id = Array.length model.players
                        , name = model.newPlayerInput
                        , team = Nothing
                        }
                in
                    updatePlayers (Array.push newPlayer model.players) { model | newPlayerInput = "" }
            else
                ( model, Cmd.none )

        ChangeTeamCountInput newValue ->
            ( { model | teamCountInput = newValue }, Cmd.none )

        AssignTeams ->
            ( model
            , Random.generate AssignTeams0
                (Util.shuffle (Array.initialize (Array.length model.players) identity))
            )

        AssignTeams0 shuffledIndices ->
            assignTeams (Array.toList shuffledIndices) model

        ClearPlayers ->
            updatePlayers Array.empty model

        RemovePlayer i ->
            updatePlayers (Util.splice i 1 Array.empty model.players) model


updatePlayers : Array Player -> Model -> ( Model, Cmd Msg )
updatePlayers newPlayers model =
    ( { model | players = newPlayers }, Cmd.none )


defaultTeamCount : String
defaultTeamCount =
    "2"


nAllTeams : Int
nAllTeams =
    List.length teams


nAllTeamsStr : String
nAllTeamsStr =
    String.fromInt nAllTeams


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


assignTeams : List Int -> Model -> ( Model, Cmd Msg )
assignTeams assignmentOrder model =
    case String.toInt model.teamCountInput of
        Nothing ->
            ( model, alert "Number of teams must be a string." )
                
        Just nTeams ->
            if nTeams < 2 || nTeams > List.length teams then
                ( model
                , alert
                ("Number of teams must be greater than or equal to "
                    ++ defaultTeamCount ++ " and less than or equal to "
                    ++ nAllTeamsStr ++ ".")
                )
            else
            let
                nPlayers =
                    Array.length model.players
            in
                if nTeams > nPlayers then
                    ( model
                    , alert "Number of teams must be less than or equal to the number of players."
                    )
                else
                    doAssignTeams nPlayers nTeams assignmentOrder model


doAssignTeams : Int -> Int -> List Int -> Model -> ( Model, Cmd Msg )
doAssignTeams nPlayers nTeams assignmentOrder model =
    let
        idealTeamSize =
            nPlayers // nTeams
            
        nResiduals =
            nPlayers
            |> modBy nTeams

        indexAssignments =
            -- Figure out each team size
            List.map2 Tuple.pair
                teams
                (List.repeat nResiduals (idealTeamSize + 1)
                ++ List.repeat (nTeams - nResiduals) idealTeamSize)

            -- Expand the assignments
            |> List.concatMap (\ ( team, nAssignments ) -> List.repeat nAssignments team)

            -- Assign the players
            |> List.map2 Tuple.pair assignmentOrder

        assignments =
            List.foldl (\ ( i, team ) players -> assignPlayer i team players)
                model.players indexAssignments
    in
        updatePlayers assignments model


assignPlayer : Int -> String -> Array Player -> Array Player
assignPlayer i newTeam players =
    case Array.get i players of
        Nothing ->
            players

        Just player ->
            Array.set i { player | team = Just newTeam } players


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
        , Keyed.node "div" [] (List.indexedMap playerView (Array.toList model.players))
        , Html.form [onSubmit AssignTeams]
            [ label []
                [ text "Number of Teams"
                , input
                [ value model.teamCountInput
                , onInput ChangeTeamCountInput
                , type_ "number"
                , Html.Attributes.min defaultTeamCount
                , Html.Attributes.max nAllTeamsStr
                ]
                []
            ]
            , button [type_ "submit"] [text "Assign Teams!"]
            ]
        ]


onRightClick : msg -> Attribute msg
onRightClick msg =
    preventDefaultOn "contextmenu" (Decode.succeed ( msg, True ))


basePlayerView : Int -> Player -> Html Msg
basePlayerView i player =
    div
        [ class "player"
        , class (Maybe.withDefault "unassigned" player.team)
        , onRightClick (RemovePlayer i)
        ]
        [text player.name]


playerView : Int -> Player -> ( String, Html Msg )
playerView i player =
    ( String.fromInt player.id, basePlayerView i player )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none