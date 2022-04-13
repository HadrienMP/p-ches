module Tests exposing (..)

import Expect
import Tempo exposing (msBetweenQuarterNotes, Tempo(..))
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Tempo"
        [ Test.test "60 beats per minute is 1 beat per second" <|
            \_ -> BPM 60 |> msBetweenQuarterNotes |> Expect.equal 250
        , Test.test "120 beats per minute is 1 beat per half second" <|
            \_ -> BPM 120 |> msBetweenQuarterNotes |> Expect.equal 125
        , Test.test "240 beats per minute is 1 beat per quarter second" <|
            \_ -> BPM 240 |> msBetweenQuarterNotes |> Expect.equal 62
        ]
