module Othello.Game exposing (Game, State(..), UpdateGame, scores, start, state)

import Data.Function exposing (Thunk)
import Data.TwoDMap as TwoDMap
import Maybe.Extra exposing (orElseLazy)
import Othello.Board as Board exposing (Board, Position, startingBoard)
import Othello.Cell exposing (Cell(..), InteractiveCell(..))
import Othello.Player exposing (..)
import Othello.Scores as Scores exposing (Scores)


type alias InternalGameState =
    ( Board Cell, Maybe (Board (InteractiveCell Game)), Player )


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
    Game ( ( startingBoard, Nothing, Player1 ), [] )


state : Game -> State
state ((Game ( ( board, previewBoardMaybe, currentPlayer ) as currentState, historicalStates )) as game) =
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
                flippablePositionsFrom =
                    Board.flippablePositionsFrom { withPlayer = player, board = board }

                setCurrentPlayerAtPosition =
                    Data.Function.flip TwoDMap.updateCell (HeldBy player)

                previewInteractiveCell boardAfterMove movePosition futureFlippedPositions position cell =
                    let
                        considerOtherCell _ =
                            case cell of
                                HeldBy _ ->
                                    Game ( ( board, Nothing, player ), historicalStates )

                                Empty ->
                                    case flippablePositionsFrom position of
                                        [] ->
                                            Game ( ( board, Nothing, player ), historicalStates )

                                        positions ->
                                            let
                                                previewMap =
                                                    previewInteractiveCell ((position :: positions) |> List.foldl setCurrentPlayerAtPosition board) position positions
                                            in
                                            Game ( ( board, Just (TwoDMap.indexedMap previewMap board), player ), historicalStates )
                    in
                    if movePosition == position then
                        EmptySelectable
                            { consider =
                                always <|
                                    Game ( ( board, Nothing, player ), historicalStates )
                            , select =
                                always <|
                                    Game ( ( boardAfterMove, Nothing, otherPlayer ), currentState :: historicalStates )
                            }

                    else
                        ReadOnly
                            { cell = cell
                            , marked = List.member position futureFlippedPositions
                            , consider = considerOtherCell
                            }

                interactiveCell : Position -> Cell -> InteractiveCell Game
                interactiveCell position cell =
                    case cell of
                        HeldBy _ ->
                            ReadOnly { cell = cell, marked = False, consider = always game }

                        Empty ->
                            case flippablePositionsFrom position of
                                [] ->
                                    ReadOnly { cell = cell, marked = False, consider = always game }

                                positions ->
                                    let
                                        boardAfterMove =
                                            (position :: positions) |> List.foldl setCurrentPlayerAtPosition board

                                        previewBoard =
                                            TwoDMap.indexedMap (previewInteractiveCell boardAfterMove position positions) board
                                    in
                                    EmptySelectable
                                        { consider =
                                            always <|
                                                Game ( ( board, Just previewBoard, player ), historicalStates )
                                        , select =
                                            always <|
                                                Game ( ( boardAfterMove, Nothing, otherPlayer ), currentState :: historicalStates )
                                        }

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
    case previewBoardMaybe of
        Just interactiveBoard ->
            InProgress
                { board = interactiveBoard
                , player = currentPlayer
                , revertMaybe = revertMaybe
                }

        Nothing ->
            (attemptInProgressStateForPlayer currentPlayer |> orElseLazy (\_ -> attemptInProgressStateForPlayer otherPlayer))
                |> Maybe.withDefault (GameOver { board = board, revert = revert })


scores : Game -> Scores
scores (Game ( ( board, _, _ ), _ )) =
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
