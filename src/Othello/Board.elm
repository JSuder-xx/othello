module Othello.Board exposing (Board, BoardDelta, BoardDimension, BoardIndex, BoardPosition, Row, anyCell, deltaPosition, deltas, getPosition, mapPosition, setPosition)

import Array
import Data.List
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index
import StaticArray.Length as Length


type alias BoardDimension =
    Index.Eight


type alias BoardIndex =
    Index.Index BoardDimension


type alias BoardDelta =
    ( BoardIndex -> Maybe BoardIndex, BoardIndex -> Maybe BoardIndex )


type alias BoardPosition =
    ( BoardIndex, BoardIndex )


type alias Row cell =
    StaticArray BoardDimension cell


type alias Board cell =
    StaticArray BoardDimension (Row cell)


deltaPosition : BoardDelta -> BoardPosition -> Maybe BoardPosition
deltaPosition ( rowDelta, colDelta ) ( row, col ) =
    Maybe.map2 Tuple.pair (rowDelta row) (colDelta col)


{-| ATTENTION: Implemented this because of a bug in StaticArray! I wish I had just rolled by own because StaticArray
implements Peano numbers (which should be their own library and not coupled to SA) and apparently has at least one major bug.
-}
indexedMap : (BoardIndex -> a -> b) -> StaticArray BoardDimension a -> StaticArray BoardDimension b
indexedMap fn arr =
    let
        len =
            StaticArray.length arr

        head =
            StaticArray.get Index.first arr

        values =
            arr |> StaticArray.toArray

        get idx =
            case Array.get idx values of
                Nothing ->
                    fn Index.first head

                Just v ->
                    fn (Index.fromModBy len idx) v
    in
    StaticArray.initialize len get


mapPosition : (BoardPosition -> a -> b) -> Board a -> Board b
mapPosition fn =
    let
        mapRow rowIndex =
            indexedMap (\colIndex -> \cell -> fn ( rowIndex, colIndex ) cell)
    in
    indexedMap mapRow


setPosition : BoardPosition -> cell -> Board cell -> Board cell
setPosition ( rowIndex, colIndex ) updateWith =
    let
        map ( r, c ) cell =
            if r == rowIndex && c == colIndex then
                updateWith

            else
                cell
    in
    mapPosition map


getPosition : BoardPosition -> Board cell -> cell
getPosition ( r, c ) =
    StaticArray.get r >> StaticArray.get c


anyCell : (a -> Bool) -> Board a -> Bool
anyCell p =
    StaticArray.toList >> List.concatMap StaticArray.toList >> List.any p


{-| The eight deltas posible.
-}
deltas : List BoardDelta
deltas =
    [ ( -1, Index.decrease )
    , ( 0, Just )
    , ( 1, Index.increase Length.eight )
    ]
        |> Data.List.cartesianProduct
        |> List.filter (\( ( r, _ ), ( c, _ ) ) -> r /= 0 || c /= 0)
        |> List.map (\( ( _, r ), ( _, c ) ) -> ( r, c ))
