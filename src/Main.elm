module Main exposing (..)

import Browser
import Html exposing (Html, a, div, h3, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter)
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


tileColor : String
tileColor =
    "#060"


tileView : (a -> Html UpdateGame) -> a -> Html UpdateGame
tileView contentsView contents =
    span
        [ style "width" cellSize
        , style "height" cellSize
        , style "display" "inline-block"
        , style "border-top" "solid 1px #131"
        , style "border-left" "solid 1px #131"
        , style "border-right" "solid 1px #353"
        , style "border-bottom" "solid 1px #353"
        , style "background-color" tileColor
        , style "margin-right" "1px"
        , style "margin-bottom" "1px"
        , style "padding" "8px"
        , style "font-size" "36px"
        , style "text-align" "center"
        , style "vertical-align" "middle"
        ]
        [ contentsView contents
        ]


discView : String -> { marked : Bool, hover : msg, selectMaybe : Maybe msg } -> Html msg
discView color { marked, hover, selectMaybe } =
    let
        dimension =
            if marked then
                "calc(100% - 6px)"

            else
                "100%"
    in
    div
        (List.concat
            [ selectMaybe |> Maybe.map (onClick >> List.singleton) |> Maybe.withDefault []
            , [ hover |> onMouseEnter
              , style "width" dimension
              , style "height" dimension
              , style "border-radius" "50%"
              , style "background-color" color
              , style "border"
                    (if marked then
                        "solid 3px #22a"

                     else
                        "0"
                    )
              ]
            ]
        )
        []


cellContentsView : Bool -> UpdateGame -> Cell -> Html UpdateGame
cellContentsView marked hover cell =
    case cell of
        Empty ->
            discView tileColor { marked = False, hover = hover, selectMaybe = Nothing }

        HeldBy player ->
            discView
                (case player of
                    Player1 ->
                        "black"

                    Player2 ->
                        "white"
                )
                { marked = marked, hover = hover, selectMaybe = Nothing }


cellView : UpdateGame -> Cell -> Html UpdateGame
cellView =
    cellContentsView False >> tileView


interactiveCellView : InteractiveCell Game -> Html UpdateGame
interactiveCellView =
    let
        interactiveCellContentsView interactiveCell =
            case interactiveCell of
                ReadOnly { cell, consider, marked } ->
                    cellContentsView marked consider cell

                EmptySelectable { consider, select } ->
                    discView "#aaa" { marked = False, selectMaybe = Just select, hover = consider }
    in
    tileView interactiveCellContentsView


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
    div [ style "padding" "8px" ]
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
                    , boardView board (cellView (always game))
                    ]
        , scoresView scores
        ]
