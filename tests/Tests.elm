module Tests exposing (..)

import Expect
import Tempo exposing (Tempo(..), msBetweenSixteenthNotes)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Tempo"
        [ Test.test "60 beats per minute is 1 beat per second, so one sixteenth note every quarter second" <|
            \_ -> BPM 60 |> msBetweenSixteenthNotes |> Expect.equal 250
        , Test.test "30 beats per minute is 1 beat per second, so one sixteenth note every half second" <|
            \_ -> BPM 30 |> msBetweenSixteenthNotes |> Expect.equal 500
        , Test.test "120 beats per minute is 1 beat per half second" <|
            \_ -> BPM 120 |> msBetweenSixteenthNotes |> Expect.equal 125
        , Test.test "240 beats per minute is 1 beat per quarter second" <|
            \_ -> BPM 240 |> msBetweenSixteenthNotes |> Expect.equal 62
        ]
