module Main exposing (..)

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
    , field : List Int
    , lastSown : Maybe Int -- Point out last position of sowing
    , winner : Winner
    }


type alias Model =
    Stage


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { turn = Myself, field = [ 0, 4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4 ], lastSown = Nothing, winner = Playing }, Cmd.none )



--Sow stones in a hole to other holes conterclockwise


sowStone : Int -> Stage -> Stage
sowStone position stage =
    let
        num =
            getAtStone position stage

        sownList =
            List.repeat 14 0
    in
    if modBy 7 position == 0 then
        stage
        -- If makeSpread occur at kalah hole, sowing does not occur.

    else
        let
            newField =
                List.indexedMap
                    (\i n ->
                        if modBy 14 (i - position) <= modBy 14 num && modBy 14 (i - position) /= 0 then
                            n + 1 + num // 14

                        else
                            n + num // 14
                    )
                    (sownList |> Array.fromList |> Array.set position (-1 * num) |> Array.toList)
                    |> List.map2 (+) stage.field

            lastPosition =
                modBy 14 (num + position)
        in
        { stage | field = newField, lastSown = Just lastPosition }


captureStone : Stage -> Stage
captureStone stage =
    let
        capturedList =
            List.repeat 14 0

        aimedGoal =
            if stage.turn == Myself then
                7

            else
                0

        oppositeStone =
            case stage.lastSown of
                Nothing ->
                    0

                Just i ->
                    getAtStone (14 - i) stage

        lastSownInt =
            case stage.lastSown of
                Nothing ->
                    -1

                Just i ->
                    i
    in
    if oppositeStone == 0 then
        stage

    else if lastSownInt >= modBy 14 (aimedGoal + 8) && lastSownInt <= modBy 14 (aimedGoal + 13) && getAtStone lastSownInt stage == 1 then
        let
            newField =
                capturedList
                    |> Array.fromList
                    |> Array.set aimedGoal oppositeStone
                    |> Array.set (14 - lastSownInt) (-1 * oppositeStone)
                    |> Array.toList
                    |> List.map2 (+) stage.field
        in
        { stage | field = newField }

    else
        stage


setNextTurn : Stage -> Stage
setNextTurn stage =
    if stage.turn == Myself then
        if stage.lastSown == Just 7 then
            { stage | turn = Myself, lastSown = Nothing }

        else
            { stage | turn = Opposite, lastSown = Nothing }

    else if stage.lastSown == Just 0 then
        { stage | turn = Opposite, lastSown = Nothing }

    else
        { stage | turn = Myself, lastSown = Nothing }


checkWinner : Stage -> Stage
checkWinner stage =
    let
        field =
            Array.fromList stage.field
    in
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
            { stage | winner = Draw }

        else if myEndStone + myOnRoadStone > oppositeEndStone + oppositeOnRoadStone then
            { stage | winner = End Myself }

        else
            { stage | winner = End Opposite }

    else
        { stage | winner = Playing }


spreadStone : Int -> Stage -> Stage
spreadStone position stage =
    let
        num =
            getAtStone position stage
    in
    if num == 0 then
        stage

    else
        stage
            |> sowStone position
            |> captureStone
            |> setNextTurn
            |> checkWinner


getAtStone : Int -> Stage -> Int
getAtStone position stage =
    let
        arrayField =
            Array.fromList stage.field
    in
    case Array.get position arrayField of
        Nothing ->
            0

        Just i ->
            i


getSummary : Stage -> { my : Int, opposite : Int }
getSummary stage =
    let
        myEndStone =
            getAtStone 7 stage

        oppositeEndStone =
            getAtStone 0 stage

        myOnRoadStone =
            List.sum <| Array.toList <| Array.slice 1 7 (Array.fromList stage.field)

        oppositeOnRoadStone =
            List.sum <| Array.toList <| Array.slice 8 14 (Array.fromList stage.field)
    in
    { my = myEndStone + myOnRoadStone, opposite = oppositeEndStone + oppositeOnRoadStone }


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
    let
        summary =
            getSummary model
    in
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
                        ++ " ("
                        ++ String.fromInt summary.my
                        ++ " vs "
                        ++ String.fromInt summary.opposite
                        ++ " )"
        )


main : Program () Model Msg
main =
    Browser.element
        { init = always initialModel
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
