module Tempo exposing (..)


type Tempo
    = BPM Int


getBpm : Tempo -> Int
getBpm tempo =
    case tempo of
        BPM bpm ->
            bpm


msBetweenSixteenthNotes : Tempo -> Int
msBetweenSixteenthNotes tempo =
    let
        quarterNotesPerMinute = getBpm tempo
        sixteenthNotesPerMinute = quarterNotesPerMinute * 4
        sixteenthNotesPerSecond = sixteenthNotesPerMinute // 60
        secondsBetweenSixteenth = 1 / toFloat sixteenthNotesPerSecond
        -- secondsBetweenSixteenth = minutesBetweenSixteenth * 60
        msBetweenSixteenth = secondsBetweenSixteenth * 1000
    in
    floor msBetweenSixteenth
