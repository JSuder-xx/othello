module Main exposing (..)

import Browser
import Html exposing (Html, a, div, h3, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Othello.Board exposing (Board)
import Othello.Cell exposing (Cell(..), InteractiveCell(..))
import Othello.Game exposing (Game, State(..), UpdateGame)
import Othello.Player exposing (..)
import Othello.Scores as Scores exposing (Scores)
import StaticArray exposing (StaticArray)


main : Program () Game UpdateGame
main =
    Browser.element
        { init =
            \() -> ( Othello.Game.start, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : UpdateGame -> Game -> ( Game, Cmd UpdateGame )
update fn _ =
    ( fn (), Cmd.none )


cellSize : String
cellSize =
    "64px"


contentsSize : String
contentsSize =
    "48px"


basicCellView : (a -> Html UpdateGame) -> a -> Html UpdateGame
basicCellView contentsView contents =
    span
        [ style "width" cellSize
        , style "height" cellSize
        , style "display" "inline-block"
        , style "border-top" "solid 1px #131"
        , style "border-left" "solid 1px #131"
        , style "border-right" "solid 1px #353"
        , style "border-bottom" "solid 1px #353"
        , style "background-color" "#060"
        , style "margin-right" "1px"
        , style "margin-bottom" "1px"
        , style "padding" "8px"
        , style "font-size" "36px"
        , style "text-align" "center"
        , style "vertical-align" "middle"
        ]
        [ contentsView contents
        ]


tileView : String -> Maybe msg -> Html msg
tileView color clickMaybe =
    div
        (List.concat
            [ clickMaybe |> Maybe.map (onClick >> List.singleton) |> Maybe.withDefault []
            , [ style "width" "100%"
              , style "height" "100%"
              , style "border-radius" "50%"
              , style "background-color" color
              ]
            ]
        )
        []


cellContentsView : Cell -> Html msg
cellContentsView cell =
    case cell of
        Empty ->
            text ""

        HeldBy player ->
            tileView
                (case player of
                    Player1 ->
                        "black"

                    Player2 ->
                        "white"
                )
                Nothing


cellView : Cell -> Html UpdateGame
cellView =
    basicCellView cellContentsView


interactiveCellView : InteractiveCell Game -> Html UpdateGame
interactiveCellView =
    let
        interactiveCellContentsView interactiveCell =
            case interactiveCell of
                ReadOnly cell ->
                    cellContentsView cell

                EmptySelectable gameUpdate ->
                    tileView "#aaa" (Just gameUpdate)
    in
    basicCellView interactiveCellContentsView


keyedStaticArray : (a -> b) -> StaticArray n a -> List ( String, b )
keyedStaticArray itemView =
    StaticArray.toList >> List.indexedMap (\index -> \item -> ( String.fromInt index, itemView item ))


boardView : Board a -> (a -> Html UpdateGame) -> Html UpdateGame
boardView board cell =
    let
        rowView row =
            Keyed.node "div" [] (row |> keyedStaticArray cell)
    in
    Keyed.node "div" [] (board |> keyedStaticArray rowView)


updateGameView : String -> UpdateGame -> Html UpdateGame
updateGameView str upd =
    a [ style "color" "blue", onClick upd ] [ text str ]


undoView : UpdateGame -> Html UpdateGame
undoView =
    updateGameView "Undo"


emptySpan : Html msg
emptySpan =
    span [] [ text "." ]


playerName : Player -> String
playerName player =
    case player of
        Player1 ->
            "One"

        Player2 ->
            "Two"


scoresView : Scores -> Html UpdateGame
scoresView scores =
    div []
        [ div [] [ [ "Player One:", String.fromInt <| Scores.player1 scores ] |> String.join "" |> text ]
        , div [] [ [ "Player Two:", String.fromInt <| Scores.player2 scores ] |> String.join "" |> text ]
        ]


view : Game -> Html UpdateGame
view game =
    let
        scores =
            Othello.Game.scores game
    in
    div [ style "padding" "6px" ]
        [ case Othello.Game.state game of
            InProgress { board, player, revertMaybe } ->
                div []
                    [ h3 []
                        [ [ "Player ", playerName player, "'s Turn" ] |> String.concat |> text ]
                    , revertMaybe |> Maybe.map undoView |> Maybe.withDefault (text "")
                    , boardView board interactiveCellView
                    ]

            GameOver { board, revert } ->
                div []
                    [ h3 []
                        [ text <|
                            case Scores.compare1To2 scores of
                                LT ->
                                    "Player Two Won!!!"

                                GT ->
                                    "Player One Won!!!"

                                EQ ->
                                    "DRAW!!!"
                        ]
                    , undoView revert
                    , boardView board cellView
                    ]
        , scoresView scores
        ]
