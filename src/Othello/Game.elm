module Othello.Game exposing (Cell(..), Game, InteractiveCell(..), Scores, State(..), UpdateGame, scores, start, state)

import Data.Function
import Maybe.Extra exposing (orElseLazy)
import Othello.Board exposing (..)
import Othello.Player exposing (..)
import StaticArray
import StaticArray.Length as Length


{-| A cell is either empty or currently held by a player.
-}
type Cell
    = Empty
    | HeldBy Player


{-| An interactive cell type is either actually read-only or it is an empty selectable location.
-}
type InteractiveCell
    = EmptySelectable UpdateGame
    | ReadOnly Cell


type alias InternalGameState =
    ( Board Cell, Player )


type alias Scores =
    { player1 : Int, player2 : Int }


type Game
    = Game ( InternalGameState, List InternalGameState )


{-| A thunk that returns an updated game.
-}
type alias UpdateGame =
    () -> Game


{-| Each case consists of a record of actions appropriate to that case; hypermedia style.
-}
type State
    = InProgress
        { board : Board InteractiveCell
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
    let
        eightArray =
            StaticArray.initialize Length.eight

        emptyRow : Row Cell
        emptyRow =
            eightArray <| always Empty

        eightArrayEEEABEEE e a b =
            eightArray
                (\index ->
                    if (index <= 2) || (index >= 5) then
                        e

                    else if index == 3 then
                        a

                    else
                        b
                )

        p1p2Row =
            eightArrayEEEABEEE Empty (HeldBy Player1) (HeldBy Player2)

        p2p1Row =
            eightArrayEEEABEEE Empty (HeldBy Player2) (HeldBy Player1)

        startingBoard =
            eightArrayEEEABEEE emptyRow p1p2Row p2p1Row
    in
    Game ( ( startingBoard, Player1 ), [] )


isEmptySelectable : InteractiveCell -> Bool
isEmptySelectable c =
    case c of
        EmptySelectable _ ->
            True

        _ ->
            False


flippablePositions : { currentPlayer : Player, fromPosition : BoardPosition, board : Board Cell } -> BoardDelta -> List BoardPosition
flippablePositions { currentPlayer, fromPosition, board } delta =
    let
        get =
            Data.Function.flip getPosition board

        recurse : List BoardPosition -> Maybe BoardPosition -> List BoardPosition
        recurse acc posMaybe =
            case posMaybe of
                Nothing ->
                    []

                Just newPosition ->
                    case get newPosition of
                        Empty ->
                            []

                        HeldBy p ->
                            if p == currentPlayer then
                                acc

                            else
                                recurse (newPosition :: acc) (deltaPosition delta newPosition)
    in
    recurse [] (deltaPosition delta fromPosition)


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

        mapCell : BoardPosition -> Cell -> InteractiveCell
        mapCell position cell =
            case cell of
                HeldBy _ ->
                    ReadOnly cell

                Empty ->
                    case List.concatMap (flippablePositions { currentPlayer = currentPlayer, fromPosition = position, board = board }) deltas of
                        [] ->
                            ReadOnly cell

                        positions ->
                            EmptySelectable <|
                                always <|
                                    let
                                        newBoard =
                                            (position :: positions) |> List.foldl (Data.Function.flip setPosition (HeldBy currentPlayer)) board
                                    in
                                    Game ( ( newBoard, otherPlayer ), currentState :: historicalStates )

        inProgressStateForPlayer player =
            let
                newBoard =
                    mapPosition mapCell board
            in
            if anyCell isEmptySelectable newBoard then
                Just <|
                    InProgress
                        { board = newBoard
                        , player = player
                        , revertMaybe = revertMaybe
                        }

            else
                Nothing
    in
    (inProgressStateForPlayer currentPlayer |> orElseLazy (\_ -> inProgressStateForPlayer otherPlayer))
        |> Maybe.withDefault (GameOver { board = board, revert = revert })


scores : Game -> Scores
scores (Game ( ( board, _ ), _ )) =
    let
        foldCell cell totals =
            case cell of
                Empty ->
                    totals

                HeldBy p ->
                    case p of
                        Player1 ->
                            { totals | player1 = totals.player1 + 1 }

                        Player2 ->
                            { totals | player2 = totals.player2 + 1 }

        foldRow row totals =
            row |> StaticArray.toList |> List.foldl foldCell totals
    in
    board |> StaticArray.toList |> List.foldl foldRow { player1 = 0, player2 = 0 }
