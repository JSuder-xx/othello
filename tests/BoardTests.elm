module BoardTests exposing (..)

import Data.TwoDMap as TwoDMap
import Expect
import Othello.Board as Board exposing (Board, Delta)
import StaticArray
import StaticArray.Index as Index exposing (Index)
import StaticArray.Length as Length
import Test exposing (..)


origin : Board.Position
origin =
    ( Index.first, Index.first )


board : Board ( Int, Int )
board =
    StaticArray.initialize Length.eight (\row -> StaticArray.initialize Length.eight (\col -> ( row, col )))


increase : Index Index.Eight -> Maybe (Index Index.Eight)
increase =
    Index.increase Length.eight


left : Delta
left =
    ( Just, Index.decrease )


right : Delta
right =
    ( Just, increase )


up : Delta
up =
    ( Index.decrease, Just )


down : Delta
down =
    ( increase, Just )


suite : Test
suite =
    describe "Board" <|
        case Index.range Length.eight of
            [ one, two, three, four, five, _, seven, eight ] ->
                [ describe "delta" <|
                    let
                        expectPosition delta position expected _ =
                            case TwoDMap.deltaPosition delta position of
                                Just actual ->
                                    Expect.equal actual expected

                                Nothing ->
                                    Expect.fail "Expected"

                        expectFailed delta position _ =
                            case TwoDMap.deltaPosition delta position of
                                Just _ ->
                                    Expect.fail "Did not expect a value"

                                Nothing ->
                                    Expect.pass
                    in
                    [ test "right of origin" <|
                        expectPosition right origin ( one, two )
                    , test "down of origin" <|
                        expectPosition down origin ( two, one )
                    , test "down of (7, 2)" <|
                        expectPosition down ( seven, two ) ( eight, two )
                    , test "right of (1, 7)" <|
                        expectPosition right ( one, seven ) ( one, eight )
                    , test "right of (1, 8)" <|
                        expectFailed right ( one, eight )
                    , test "left of origin" <|
                        expectFailed left origin
                    , test "up of origin" <|
                        expectFailed up origin
                    ]
                , describe "getPosition" <|
                    let
                        expectPosition boardPosition value _ =
                            Expect.equal (TwoDMap.cell boardPosition board) value
                    in
                    [ test "(1, 1)" <|
                        expectPosition ( one, one ) ( 0, 0 )
                    , test "(4, 5)" <|
                        expectPosition ( four, five ) ( 3, 4 )
                    , test "(8, 8)" <|
                        expectPosition ( eight, eight ) ( 7, 7 )
                    ]
                , describe "mapPosition" <|
                    let
                        expectLocations map position expectedValue _ =
                            Expect.equal (TwoDMap.cell position (TwoDMap.indexedMap map board)) expectedValue

                        flip ( row, col ) _ =
                            ( Index.toInt row, Index.toInt col )
                    in
                    [ test "flip (1, 1)" <|
                        expectLocations flip ( one, one ) ( 0, 0 )
                    , test "flip (3, 3)" <|
                        expectLocations flip ( three, three ) ( 2, 2 )
                    ]
                , describe "setPosition" <|
                    let
                        expectPosition boardPosition value _ =
                            Expect.equal (TwoDMap.updateCell boardPosition value board |> TwoDMap.cell boardPosition) value
                    in
                    [ test "(1, 1)" <|
                        expectPosition ( one, one ) ( 3, 5 )
                    , test "(4, 5)" <|
                        expectPosition ( four, five ) ( 13, 22 )
                    , test "(8, 8)" <|
                        expectPosition ( eight, eight ) ( 17, 17 )
                    ]
                ]

            _ ->
                []
