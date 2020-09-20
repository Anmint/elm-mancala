module Example exposing (..)

import Array exposing (Array)
import Element.Region exposing (description)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Update Stage"
        [ describe "makeSpread"
            [ describe "progressSowing"
                [ test "no stone, no sow" <|
                    \_ ->
                        let
                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], lastSown = Just 3, remainStone = 0, winner = Playing }
                        in
                        Expect.equal initialmodel (progressSowing initialmodel)
                , test "no stone, no sow 2" <|
                    \_ ->
                        let
                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], lastSown = Nothing, remainStone = 6, winner = Playing }
                        in
                        Expect.equal initialmodel (progressSowing initialmodel)
                , test "sow only one stone" <|
                    \_ ->
                        let
                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], lastSown = Just 3, remainStone = 3, winner = Playing }

                            sownmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 1, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], lastSown = Just 4, remainStone = 2, winner = Playing }
                        in
                        Expect.equal sownmodel (progressSowing initialmodel)
                , test "If stone is sown in the last pit, next will be back to zero" <|
                    \_ ->
                        let
                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], lastSown = Just 13, remainStone = 3, winner = Playing }

                            sownmodel =
                                { turn = Myself, field = [ 1, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], lastSown = Just 0, remainStone = 2, winner = Playing }
                        in
                        Expect.equal sownmodel (progressSowing initialmodel)
                ]
            , describe "sowStone"
                [ test "no stone, no sow" <|
                    \_ ->
                        let
                            noStone =
                                [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ]

                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 14, 0, 0, 0, 5, 0, 0 ], lastSown = Nothing, remainStone = 2, winner = Playing }
                        in
                        Expect.equal noStone (sowStone 4 initialmodel).field
                , test "normal sowing" <|
                    \_ ->
                        let
                            normalSowing =
                                [ 0, 0, 0, 1, 1, 1, 1, 0, 14, 0, 0, 5, 0, 0 ]

                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], lastSown = Nothing, remainStone = 2, winner = Playing }
                        in
                        Expect.equal normalSowing (sowStone 2 initialmodel).field
                , test "circled sowing" <|
                    \_ ->
                        let
                            circledSowing =
                                [ 1, 1, 5, 0, 0, 0, 0, 0, 14, 0, 0, 0, 1, 1 ]

                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], lastSown = Nothing, remainStone = 2, winner = Playing }
                        in
                        Expect.equal circledSowing (sowStone 11 initialmodel).field
                , test "overlapped sowing" <|
                    \_ ->
                        let
                            overlappedSowing =
                                [ 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 6, 1, 1 ]

                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 0, 0, 0, 0, 14, 0, 0, 5, 0, 0 ], lastSown = Nothing, remainStone = 2, winner = Playing }
                        in
                        Expect.equal overlappedSowing (sowStone 8 initialmodel).field
                ]
            , describe "captureStone"
                [ test "Capture opposite stone" <|
                    \_ ->
                        let
                            capturedField =
                                [ 0, 0, 4, 0, 1, 0, 0, 3, 14, 0, 0, 5, 0, 0 ]

                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 1, 0, 0, 0, 14, 0, 3, 5, 0, 0 ], lastSown = Just 4, remainStone = 2, winner = Playing }
                        in
                        Expect.equal capturedField (captureStone initialmodel).field
                , test "The last stone is sown in the opposite side, capture will not occur" <|
                    \_ ->
                        let
                            notCapturedField =
                                [ 1, 1, 4, 1, 1, 0, 0, 0, 14, 0, 3, 5, 0, 1 ]

                            initialmodel =
                                { turn = Opposite, field = [ 1, 1, 4, 1, 1, 0, 0, 0, 14, 0, 3, 5, 0, 1 ], lastSown = Just 4, remainStone = 2, winner = Playing }
                        in
                        Expect.equal notCapturedField (captureStone initialmodel).field
                , test "The last stone is sown in a goal pit, capture will not occur" <|
                    \_ ->
                        let
                            notCapturedField =
                                [ 1, 0, 4, 0, 0, 0, 0, 4, 14, 0, 3, 5, 0, 1 ]

                            initialmodel =
                                { turn = Opposite, field = [ 1, 0, 4, 0, 0, 0, 0, 4, 14, 0, 3, 5, 0, 1 ], lastSown = Just 0, remainStone = 2, winner = Playing }
                        in
                        Expect.equal notCapturedField (captureStone initialmodel).field
                ]
            , describe "setNextTurn"
                [ test "Continue player's turn if you sown to your goal pit" <|
                    \_ ->
                        let
                            whoIsNextTurn =
                                Myself

                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 1, 1, 1, 1, 14, 0, 3, 5, 0, 0 ], lastSown = Just 7, remainStone = 2, winner = Playing }
                        in
                        Expect.equal whoIsNextTurn (setNextTurn initialmodel).turn
                , test "If not, next turn is opposite" <|
                    \_ ->
                        let
                            whoIsNextTurn =
                                Opposite

                            initialmodel =
                                { turn = Myself, field = [ 0, 0, 4, 0, 1, 1, 1, 0, 14, 0, 3, 5, 0, 0 ], lastSown = Just 6, remainStone = 2, winner = Playing }
                        in
                        Expect.equal whoIsNextTurn (setNextTurn initialmodel).turn
                ]
            ]
        ]
