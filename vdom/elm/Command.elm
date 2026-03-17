module Command exposing (Command(..), encoder, needsAck)

import Bytes.Encode
import Node exposing (Node)


type Command
    = SetRootNode Node


commandTag : Command -> Int
commandTag command =
    case command of
        SetRootNode _ ->
            1


needsAck : Command -> Bool
needsAck command =
    case command of
        SetRootNode _ ->
            True


encoder : Command -> Bytes.Encode.Encoder
encoder command =
    [ [ Bytes.Encode.unsignedInt8 (commandTag command) ]
    , case command of
        SetRootNode node ->
            [ Node.encoder node ]
    ]
        |> List.concat
        |> Bytes.Encode.sequence
