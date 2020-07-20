module Example exposing (..)

import Array exposing (Array)
import Element.Region exposing (description)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Stage -> List Int"
        [ describe "makeSpread"
            [ test "no stone, no sow" <|
                \_ ->
                    let
                        noStone =
                            [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ]

                        initialmodel =
                            { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], lastSown = Nothing, winner = Playing }
                    in
                    Expect.equal noStone (sowStone 4 initialmodel).field
            , test "normal sowing" <|
                \_ ->
                    let
                        normalSowing =
                            [ 0, 0, 0, 1, 1, 1, 1, 0, 14, 0, 0, 5, 0, 0 ]

                        initialmodel =
                            { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], lastSown = Nothing, winner = Playing }
                    in
                    Expect.equal normalSowing (sowStone 2 initialmodel).field
            , test "circled sowing" <|
                \_ ->
                    let
                        circledSowing =
                            [ 1, 1, 5, 0, 0, 0, 0, 0, 14, 0, 0, 0, 1, 1 ]

                        initialmodel =
                            { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], lastSown = Nothing, winner = Playing }
                    in
                    Expect.equal circledSowing (sowStone 11 initialmodel).field
            , test "overlapped sowing" <|
                \_ ->
                    let
                        overlappedSowing =
                            [ 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 6, 1, 1 ]

                        initialmodel =
                            { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], lastSown = Nothing, winner = Playing }
                    in
                    Expect.equal overlappedSowing (sowStone 8 initialmodel).field
            ]
        ]
