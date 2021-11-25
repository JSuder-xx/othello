module Othello.Game exposing (Game, State(..), UpdateGame, scores, start, state)

import Data.Function exposing (Thunk)
import Data.TwoDMap as TwoDMap
import Maybe.Extra exposing (orElseLazy)
import Othello.Board as Board exposing (Board, Position, startingBoard)
import Othello.Cell exposing (Cell(..), InteractiveCell(..))
import Othello.Player exposing (..)
import Othello.Scores as Scores exposing (Scores)


type alias InternalGameState =
    ( Board Cell, Player )


type Game
    = Game ( InternalGameState, List InternalGameState )


{-| A thunk that returns an updated game.
-}
type alias UpdateGame =
    Thunk Game


{-| Each case consists of a record of actions appropriate to that case; hypermedia style.
-}
type State
    = InProgress
        { board : Board (InteractiveCell Game)
        , player : Player
        , revertMaybe : Maybe UpdateGame
        }
    | GameOver
        { board : Board Cell
        , revert : UpdateGame
        }


{-| A brand new game of Othello.
-}
start : Game
start =
    Game ( ( startingBoard, Player1 ), [] )


state : Game -> State
state ((Game ( ( board, currentPlayer ) as currentState, historicalStates )) as game) =
    let
        otherPlayer =
            togglePlayer currentPlayer

        revertMaybe : Maybe UpdateGame
        revertMaybe =
            case historicalStates of
                mostRecent :: beforeThat ->
                    Just <| always <| Game ( mostRecent, beforeThat )

                [] ->
                    Nothing

        revert =
            revertMaybe |> Maybe.withDefault (always game)

        attemptInProgressStateForPlayer player =
            let
                boardFromPlayingPosition =
                    Board.boardFromPlayingPosition { withPlayer = player, board = board }

                interactiveCell : Position -> Cell -> InteractiveCell Game
                interactiveCell position cell =
                    case cell of
                        HeldBy _ ->
                            ReadOnly cell

                        Empty ->
                            case boardFromPlayingPosition position of
                                Nothing ->
                                    ReadOnly cell

                                Just boardAfterMove ->
                                    EmptySelectable <|
                                        always <|
                                            Game ( ( boardAfterMove, otherPlayer ), currentState :: historicalStates )

                interactiveBoard =
                    TwoDMap.indexedMap interactiveCell board
            in
            if Board.hasMove interactiveBoard then
                Just <|
                    InProgress
                        { board = interactiveBoard
                        , player = player
                        , revertMaybe = revertMaybe
                        }

            else
                Nothing
    in
    (attemptInProgressStateForPlayer currentPlayer |> orElseLazy (\_ -> attemptInProgressStateForPlayer otherPlayer))
        |> Maybe.withDefault (GameOver { board = board, revert = revert })


scores : Game -> Scores
scores (Game ( ( board, _ ), _ )) =
    let
        scoreTransform cell =
            case cell of
                Empty ->
                    identity

                HeldBy p ->
                    Scores.incPlayer p
    in
    board
        |> TwoDMap.flatList
        |> List.foldl scoreTransform Scores.initial
