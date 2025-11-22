module Player exposing (Player, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

type alias Player =
    { id : Int
    , name : String
    , team : Maybe String
    }


decoder : Decoder Player
decoder =
    Decode.map3 Player
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "team" (Decode.nullable Decode.string))


encoder : Player -> Encode.Value
encoder player =
    Encode.object
        [ ( "id", Encode.int player.id )
        , ( "name", Encode.string player.name )
        ,
            ( "team"
            , case player.team of
                Just team ->
                    Encode.string team
                
                Nothing ->
                    Encode.null
            )
        ]