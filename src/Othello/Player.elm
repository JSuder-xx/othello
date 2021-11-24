module Othello.Player exposing (Player(..), togglePlayer)


type Player
    = Player1
    | Player2


togglePlayer : Player -> Player
togglePlayer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1
