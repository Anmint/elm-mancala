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
                            List.repeat 14 0

                        initialmodel =
                            { turn = Myself, field = Array.fromList [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], winner = Playing }
                    in
                    Expect.equal noStone (makeSpread 4 initialmodel)
            , test "normal sowing" <|
                \_ ->
                    let
                        normalSowing =
                            [ 0, 0, -4, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 ]

                        initialmodel =
                            { turn = Myself, field = Array.fromList [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], winner = Playing }
                    in
                    Expect.equal normalSowing (makeSpread 2 initialmodel)
            , test "circled sowing" <|
                \_ ->
                    let
                        circledSowing =
                            [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, -5, 1, 1 ]

                        initialmodel =
                            { turn = Myself, field = Array.fromList [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], winner = Playing }
                    in
                    Expect.equal circledSowing (makeSpread 11 initialmodel)
            , test "overlapped sowing" <|
                \_ ->
                    let
                        overlappedSowing =
                            [ 1, 1, 1, 1, 1, 1, 1, 1, -13, 1, 1, 1, 1, 1 ]

                        initialmodel =
                            { turn = Myself, field = Array.fromList [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], winner = Playing }
                    in
                    Expect.equal overlappedSowing (makeSpread 8 initialmodel)
            ]
        ]
