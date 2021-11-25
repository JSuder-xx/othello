module Othello.Scores exposing (Scores, compare1To2, incPlayer, incPlayer1, incPlayer2, initial, player1, player2)

import Othello.Player exposing (Player(..))


type Scores
    = Scores { player1 : Int, player2 : Int }


initial : Scores
initial =
    Scores { player1 = 0, player2 = 0 }


player1 : Scores -> Int
player1 (Scores scores) =
    scores.player1


player2 : Scores -> Int
player2 (Scores scores) =
    scores.player2


compare1To2 : Scores -> Order
compare1To2 (Scores scores) =
    compare scores.player1 scores.player2


incPlayer : Player -> Scores -> Scores
incPlayer player =
    case player of
        Player1 ->
            incPlayer1

        Player2 ->
            incPlayer2


incPlayer1 : Scores -> Scores
incPlayer1 (Scores scores) =
    Scores { scores | player1 = scores.player1 + 1 }


incPlayer2 : Scores -> Scores
incPlayer2 (Scores scores) =
    Scores { scores | player2 = scores.player2 + 1 }
