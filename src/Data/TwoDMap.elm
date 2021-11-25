module Data.TwoDMap exposing (AxisDelta, Delta, Position, Row, TwoDMap, any, cell, deltaPosition, deltas, flatList, indexedMap, updateCell)

import Array
import Data.List
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Index)
import StaticArray.Length exposing (Length)


type alias AxisDelta dimension =
    Index dimension -> Maybe (Index dimension)


type alias Delta dimension =
    ( AxisDelta dimension, AxisDelta dimension )


type alias Position dimension =
    ( Index dimension, Index dimension )


type alias Row dimension cell =
    StaticArray dimension cell


type alias TwoDMap dimension cell =
    StaticArray dimension (Row dimension cell)


deltaPosition : Delta dimension -> Position dimension -> Maybe (Position dimension)
deltaPosition ( rowDelta, colDelta ) ( row, col ) =
    Maybe.map2 Tuple.pair (rowDelta row) (colDelta col)


{-| ATTENTION: Implemented this because of a bug in StaticArray. This will be removed when the fix PR is accepted.
-}
indexedMap_ : (Index dimension -> a -> b) -> StaticArray dimension a -> StaticArray dimension b
indexedMap_ fn arr =
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


indexedMap : (Position dimension -> a -> b) -> TwoDMap dimension a -> TwoDMap dimension b
indexedMap fn =
    let
        mapRow rowIndex =
            indexedMap_ (\colIndex -> fn ( rowIndex, colIndex ))
    in
    indexedMap_ mapRow


updateCell : Position dimension -> a -> TwoDMap dimension a -> TwoDMap dimension a
updateCell ( rowIndex, colIndex ) updateWith board =
    board
        |> StaticArray.get rowIndex
        |> StaticArray.set colIndex updateWith
        |> StaticArray.set rowIndex
        |> (|>) board


cell : Position dimension -> TwoDMap dimension a -> a
cell ( r, c ) =
    StaticArray.get r >> StaticArray.get c


any : (a -> Bool) -> TwoDMap dimension a -> Bool
any p =
    StaticArray.toList >> List.concatMap StaticArray.toList >> List.any p


{-| The eight deltas posible.
-}
deltas : Length n -> List (Delta n)
deltas length =
    [ ( -1, Index.decrease )
    , ( 0, Just )
    , ( 1, Index.increase length )
    ]
        |> Data.List.cartesianProduct
        |> List.filter (\( ( r, _ ), ( c, _ ) ) -> r /= 0 || c /= 0)
        |> List.map (\( ( _, r ), ( _, c ) ) -> ( r, c ))


flatList : TwoDMap d c -> List c
flatList =
    StaticArray.toList >> List.concatMap StaticArray.toList
