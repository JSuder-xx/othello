module Othello.Cell exposing (..)

import Data.Function exposing (Thunk)
import Othello.Player exposing (Player)


{-| A cell is either empty or currently held by a player.
-}
type Cell
    = Empty
    | HeldBy Player


{-| An interactive cell type is either actually read-only or it is an empty selectable location.
-}
type InteractiveCell a
    = EmptySelectable (Thunk a)
    | ReadOnly Cell


{-| Is this an empty-selectable cell?
-}
isEmptySelectable : InteractiveCell a -> Bool
isEmptySelectable c =
    case c of
        EmptySelectable _ ->
            True

        _ ->
            False
