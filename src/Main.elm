module Main exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Bg
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)


type Player
    = Myself
    | Opposite


type Winner
    = End Player
    | Draw
    | Playing


type alias Stage =
    { turn : Player
    , field : Array Int
    , winner : Winner
    }


type alias Model =
    Stage


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { turn = Myself, field = Array.fromList [ 0, 4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4 ], winner = Playing }, Cmd.none )


spreadStone : Int -> Stage -> Stage
spreadStone position stage =
    let
        num =
            getAtStone position stage

        newfield =
            Array.set position 0 stage.field
                |> Array.toList
                |> List.map2 (+) (makeSpread position num)
                |> Array.fromList
    in
    if num == 0 then
        stage

    else
        { stage | turn = nextTurn num position stage.turn, field = newfield, winner = checkWinner newfield }


checkWinner : Array Int -> Winner
checkWinner field =
    let
        myEndStone =
            case Array.get 7 field of
                Nothing ->
                    0

                Just i ->
                    i

        oppositeEndStone =
            case Array.get 0 field of
                Nothing ->
                    0

                Just i ->
                    i

        myOnRoadStone =
            List.sum <| Array.toList <| Array.slice 1 7 field

        oppositeOnRoadStone =
            List.sum <| Array.toList <| Array.slice 8 14 field
    in
    if (myOnRoadStone == 0) || (oppositeOnRoadStone == 0) then
        if myEndStone + myOnRoadStone == oppositeEndStone + oppositeOnRoadStone then
            Draw

        else if myEndStone + myOnRoadStone > oppositeEndStone + oppositeOnRoadStone then
            End Myself

        else
            End Opposite

    else
        Playing


makeSpread : Int -> Int -> List Int
makeSpread position num =
    let
        spreadList =
            List.repeat 14 0
    in
    if modBy 7 position == 0 then
        spreadList
        -- If makeSpread occur at kalaha hole, make zero array. (Just in case)

    else
        List.indexedMap
            (\i n ->
                if modBy 14 (i - position) <= modBy 14 num && modBy 14 (i - position) /= 0 then
                    n + 1 + num // 14

                else
                    n + num // 14
            )
            spreadList


nextTurn : Int -> Int -> Player -> Player
nextTurn stoneNum position thisTurn =
    if thisTurn == Myself then
        if (7 - position) == modBy 14 stoneNum then
            Myself

        else
            Opposite

    else if (14 - position) == modBy 14 stoneNum then
        Opposite

    else
        Myself


getAtStone : Int -> Stage -> Int
getAtStone position stage =
    case Array.get position stage.field of
        Nothing ->
            0

        Just i ->
            i


type Msg
    = Spread Int
    | Initialize
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Spread position ->
            ( spreadStone position model, Cmd.none )

        Initialize ->
            initialModel

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    layout [ padding 30, spacing 4 ] (viewStage model)


viewStage : Model -> Element Msg
viewStage model =
    column [ explain Debug.todo, width fill ]
        [ el [ centerX, Font.size 32 ] (text "Mancala the World")
        , row
            [ explain Debug.todo, width fill ]
            [ viewPointCell model Opposite
            , viewSpreadField model
            , viewPointCell model Myself
            ]
        , viewProgressDesc model
        ]


viewSpreadField : Model -> Element Msg
viewSpreadField model =
    column [ explain Debug.todo, width (fillPortion 6), height fill ]
        (List.map (viewColumn model) [ Opposite, Myself ])


viewColumn : Model -> Player -> Element Msg
viewColumn model player =
    let
        targetList =
            case player of
                Myself ->
                    List.range 1 6

                Opposite ->
                    List.range 8 13 |> List.reverse
    in
    row [ explain Debug.todo, width fill, height fill ] (List.map (viewCell model) targetList)


viewCell : Model -> Int -> Element Msg
viewCell model position =
    let
        onClick =
            case model.winner of
                End _ ->
                    Events.onClick NoOp

                Draw ->
                    Events.onClick NoOp

                Playing ->
                    if ((model.turn == Myself) && (position <= 6) && getAtStone position model /= 0) || ((model.turn == Opposite) && (position >= 8) && getAtStone position model /= 0) then
                        Events.onClick (Spread position)

                    else
                        Events.onClick NoOp

        spreadable =
            case model.winner of
                End _ ->
                    mouseOver []

                Draw ->
                    mouseOver []

                Playing ->
                    if ((model.turn == Myself) && (position <= 6) && getAtStone position model /= 0) || ((model.turn == Opposite) && (position >= 8) && getAtStone position model /= 0) then
                        mouseOver [ Bg.color (rgba 1 0.2 0.3 0.5) ]

                    else
                        mouseOver []
    in
    column [ height (fill |> minimum 50), width (fill |> minimum 50), onClick, spreadable ] [ el [ centerX, centerY ] (text <| String.fromInt <| getAtStone position model) ]


viewPointCell : Model -> Player -> Element Msg
viewPointCell model player =
    case player of
        Myself ->
            column [ height (fill |> minimum 100), width (fillPortion 1 |> minimum 50) ] [ el [ centerX, centerY ] (text <| String.fromInt <| getAtStone 7 model) ]

        Opposite ->
            column [ height (fill |> minimum 100), width (fillPortion 1 |> minimum 50) ] [ el [ centerX, centerY ] (text <| String.fromInt <| getAtStone 0 model) ]


viewProgressDesc : Model -> Element Msg
viewProgressDesc model =
    el [ centerX, Font.size 32 ]
        (text <|
            case model.winner of
                Playing ->
                    (if model.turn == Myself then
                        "わたしの"

                     else
                        "相手の"
                    )
                        ++ "ターンです"

                Draw ->
                    "引き分けでした〜"

                End player ->
                    (if player == Myself then
                        "わたしの"

                     else
                        "相手の"
                    )
                        ++ "勝ちです"
        )


main : Program () Model Msg
main =
    Browser.element
        { init = always initialModel
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
