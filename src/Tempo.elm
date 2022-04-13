module Tempo exposing (..)


type Tempo
    = BPM Int


getBpm : Tempo -> Int
getBpm tempo =
    case tempo of
        BPM bpm ->
            bpm


msBetweenQuarterNotes : Tempo -> Int
msBetweenQuarterNotes tempo =
    1000 // (getBpm tempo // 60) // 4
