module Command exposing (Command(..), encoder, needsAck)

import Bytes.Encode
import Dirty exposing (dirtyTilesEncoder)
import Node exposing (Node)
import Set exposing (Set)


type Command
    = SetRootNode Node (Set ( Int, Int ))


commandTag : Command -> Int
commandTag command =
    case command of
        SetRootNode _ _ ->
            1


needsAck : Command -> Bool
needsAck command =
    case command of
        SetRootNode _ _ ->
            True


encoder : Command -> Bytes.Encode.Encoder
encoder command =
    [ [ Bytes.Encode.unsignedInt8 (commandTag command) ]
    , case command of
        SetRootNode node dirtyTiles ->
            [ Node.encoder node
            , Dirty.dirtyTilesEncoder dirtyTiles
            ]
    ]
        |> List.concat
        |> Bytes.Encode.sequence
