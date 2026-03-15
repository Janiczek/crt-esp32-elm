module Command exposing (Command(..), encoder)

import Bytes exposing (Endianness(..))
import Bytes.Encode
import Node exposing (Node)


type Command
    = GetESP32Data -- Sent by JS on init when transitioning from NotConnected to Connected.
    | SetRootNode Node


commandTag : Command -> Int
commandTag command =
    case command of
        GetESP32Data ->
            0
        SetRootNode _ ->
            1


encoder : Command -> Bytes.Encode.Encoder
encoder command =
    [ [ Bytes.Encode.unsignedInt8 (commandTag command) ]
    , case command of
        GetESP32Data -> []
        SetRootNode node ->
            [ Node.encoder node ]
    ]
        |> List.concat
        |> Bytes.Encode.sequence
