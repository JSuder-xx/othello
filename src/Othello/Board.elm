module Othello.Board exposing (Board, BoardDimension, Delta, Position, deltas, flippablePositionsFrom, hasMove, startingBoard)

import Data.Function
import Data.TwoDMap as TwoDMap exposing (TwoDMap)
import Othello.Cell exposing (Cell(..), InteractiveCell, isEmptySelectable)
import Othello.Player exposing (Player(..))
import StaticArray
import StaticArray.Index as Index
import StaticArray.Length as Length


type alias BoardDimension =
    Index.Eight


type alias Board cell =
    TwoDMap BoardDimension cell


type alias Position =
    TwoDMap.Position BoardDimension


type alias Delta =
    TwoDMap.Delta BoardDimension


startingBoard : TwoDMap BoardDimension Cell
startingBoard =
    let
        eightArray =
            StaticArray.initialize Length.eight

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
    in
    eightArrayEEEABEEE emptyRow p1p2Row p2p1Row


deltas : List Delta
deltas =
    TwoDMap.deltas Length.eight


flippablePositionsFrom : { withPlayer : Player, board : Board Cell } -> Position -> List Position
flippablePositionsFrom { withPlayer, board } fromPosition =
    let
        flippablePositions movingInDirection =
            let
                cellAtPosition =
                    Data.Function.flip TwoDMap.cell board

                move =
                    TwoDMap.deltaPosition movingInDirection

                recurse : List Position -> Maybe Position -> List Position
                recurse acc posMaybe =
                    case posMaybe of
                        Nothing ->
                            []

                        Just newPosition ->
                            case cellAtPosition newPosition of
                                Empty ->
                                    []

                                HeldBy p ->
                                    if p == withPlayer then
                                        acc

                                    else
                                        recurse (newPosition :: acc) (move newPosition)
            in
            recurse [] (move fromPosition)
    in
    deltas |> List.concatMap flippablePositions


hasMove : Board (InteractiveCell a) -> Bool
hasMove =
    TwoDMap.any isEmptySelectable
