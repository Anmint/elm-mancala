module Main exposing (..)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
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


pitNumber : Int
pitNumber =
    14


stoneNumber : Int
stoneNumber =
    4


initialModel : ( Model, Cmd Msg )
initialModel =
    ( initializeStage pitNumber stoneNumber, Cmd.none )


initializeStage : Int -> Int -> Stage
initializeStage pit stone =
    let
        initialField =
            0 :: List.repeat (pit // 2 - 1) stone |> List.append (0 :: List.repeat (pit // 2 - 1) stone)
    in
    { turn = Myself, field = initialField, lastSown = Nothing, winner = Playing }



--Sow stones in a pit to other pits conterclockwise


sowStone : Int -> Stage -> Stage
sowStone position stage =
    let
        num =
            getAtStone position stage

        sownList =
            List.repeat pitNumber 0
    in
    if modBy (pitNumber // 2) position == 0 then
        stage
        -- If makeSpread occur at kalah pit, sowing does not occur.

    else
        let
            newField =
                List.indexedMap
                    (\i n ->
                        if modBy pitNumber (i - position) <= modBy pitNumber num && modBy pitNumber (i - position) /= 0 then
                            n + 1 + num // pitNumber

                        else
                            n + num // pitNumber
                    )
                    (sownList |> Array.fromList |> Array.set position (-1 * num) |> Array.toList)
                    |> List.map2 (+) stage.field

            lastPosition =
                modBy pitNumber (num + position)
        in
        { stage | field = newField, lastSown = Just lastPosition }



-- If last stone is sown to my vacant pit and opposite pit have one or more stones, these stones are captured.


captureStone : Stage -> Stage
captureStone stage =
    let
        capturedList =
            List.repeat pitNumber 0

        aimedGoal =
            if stage.turn == Myself then
                pitNumber // 2

            else
                0

        oppositeStone =
            case stage.lastSown of
                Nothing ->
                    0

                Just i ->
                    getAtStone (pitNumber - i) stage

        lastSownInt =
            case stage.lastSown of
                Nothing ->
                    -1

                Just i ->
                    i
    in
    if oppositeStone == 0 then
        stage
        {--
    In (x,6) Kalah, index of pit are assigned like:
        0   13  12  11  10  9   8
            1   2   3   4   5   6   7
    Sum of index in each column is always 14. Generally, the sum in (x, y) kalah is always 2y + 2 (= sum of pit number).
    I have to get range of index if I get an index of goal pit (Myself = 7, Opposite = 0).
    It is notable that 8 (min of index in Opposite) + 7 (goal in Myself) == 1 (min of index in Myself) mod 14 and
    13 (max of index in Opposite) + 7 (goal in Myself) == 6 (max of index in Myself) mod 14
    More generally, consider (x, y) kalah, 
        (y + 2) (min of index in Opposite) + (y + 1) (goal in Myself) == 1 (min of index in Myself) mod (2y + 2)
        (2y + 1) (max of index in Opposite) + (y + 1) (goal in Myself) == y (max of index in Myself) mod (2y + 2)
    Inversely,
        (y + 2) (min of index in Opposite) + 0 (goal in Opposite) == (y + 2) (min of index in Opposite) mod (2y + 2)
        (2y + 1) (max of index in Opposite) + 0 (goal in Opposite) == (2y + 1) (max of index in Opposite) mod (2y + 2)
    Now pitNumber = 2y + 2, aimedGoal = y + 1.
--}

    else if lastSownInt >= modBy pitNumber (aimedGoal + pitNumber // 2 + 1) && lastSownInt <= modBy pitNumber (aimedGoal + pitNumber - 1) && getAtStone lastSownInt stage == 1 then
        let
            newField =
                capturedList
                    |> Array.fromList
                    |> Array.set aimedGoal oppositeStone
                    |> Array.set (pitNumber - lastSownInt) (-1 * oppositeStone)
                    |> Array.toList
                    |> List.map2 (+) stage.field
        in
        { stage | field = newField }

    else
        stage



-- If the last stone is sown to my goal, player's turn continue.


setNextTurn : Stage -> Stage
setNextTurn stage =
    if stage.turn == Myself then
        if stage.lastSown == Just (pitNumber // 2) then
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

        goalMyself =
            pitNumber // 2
    in
    let
        myEndStone =
            case Array.get goalMyself field of
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
            List.sum <| Array.toList <| Array.slice 1 goalMyself field

        oppositeOnRoadStone =
            List.sum <| Array.toList <| Array.slice (goalMyself + 1) pitNumber field
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



-- Get number of stone in specific pit


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
        goalMyself =
            pitNumber // 2
    in
    let
        myEndStone =
            getAtStone goalMyself stage

        oppositeEndStone =
            getAtStone 0 stage

        myOnRoadStone =
            List.sum <| Array.toList <| Array.slice 1 goalMyself (Array.fromList stage.field)

        oppositeOnRoadStone =
            List.sum <| Array.toList <| Array.slice (goalMyself + 1) pitNumber (Array.fromList stage.field)
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
    column [ spacing 10, width fill ]
        [ el [ centerX, Font.size 32 ] (text "Mancala the World")
        , row
            [ Bg.color (rgb255 102 51 51), Border.rounded 20, padding 20, spacing 5, width fill ]
            [ viewPointCell model Opposite
            , viewSpreadField model
            , viewPointCell model Myself
            ]
        , viewProgressDesc model
        ]


viewSpreadField : Model -> Element Msg
viewSpreadField model =
    column [ spacing 10, width (fillPortion 6), height fill ]
        (List.map (viewColumn model) [ Opposite, Myself ])


viewColumn : Model -> Player -> Element Msg
viewColumn model player =
    let
        targetList =
            case player of
                Myself ->
                    List.range 1 (pitNumber // 2 - 1)

                Opposite ->
                    List.range (pitNumber // 2 + 1) (pitNumber - 1) |> List.reverse
    in
    row [ spacing 10, width fill, height fill ] (List.map (viewCell model) targetList)


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
                    if ((model.turn == Myself) && (position <= (pitNumber // 2 - 1)) && getAtStone position model /= 0) || ((model.turn == Opposite) && (position >= (pitNumber // 2 + 1)) && getAtStone position model /= 0) then
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
                    if ((model.turn == Myself) && (position <= (pitNumber // 2 - 1)) && getAtStone position model /= 0) || ((model.turn == Opposite) && (position >= (pitNumber // 2 + 1)) && getAtStone position model /= 0) then
                        mouseOver [ Bg.color (rgba 1 0.2 0.3 0.5) ]

                    else
                        mouseOver []
    in
    column [ Bg.color (rgb255 153 51 51), Border.solid, Border.color (rgb 0 0 0), Border.width 1, Border.rounded 10, height (fill |> minimum 100), width (fill |> minimum 100), onClick, spreadable ] [ el boardTextSetting (text <| String.fromInt <| getAtStone position model) ]


viewPointCell : Model -> Player -> Element Msg
viewPointCell model player =
    let
        pointText =
            case player of
                Myself ->
                    text <| String.fromInt <| getAtStone (pitNumber // 2) model

                Opposite ->
                    text <| String.fromInt <| getAtStone 0 model
    in
    column [ Bg.color (rgb255 153 51 51), Border.solid, Border.color (rgb 0 0 0), Border.width 1, Border.rounded 10, height (fill |> minimum 100), width (fillPortion 1 |> minimum 50) ] [ el boardTextSetting pointText ]


boardTextSetting : List (Attribute msg)
boardTextSetting =
    [ centerX, centerY, Font.color (rgb 1 1 1) ]


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
