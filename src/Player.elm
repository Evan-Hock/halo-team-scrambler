module Player exposing (Player(..), decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

type Player
    = Unassigned String
    | Assigned String String


decoder : Decoder Player
decoder =
    Decode.map2 ( \ name mTeam ->
        case mTeam of
            Nothing ->
                Unassigned name
                
            Just team ->
                Assigned name team)
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.field "team" Decode.string))


encoder : Player -> Encode.Value
encoder player =
    case player of
        Unassigned name ->
            Encode.object [( "name", Encode.string name )]
        
        Assigned name team ->
            Encode.object
                [ ( "name", Encode.string name )
                , ( "team", Encode.string team )
                ]