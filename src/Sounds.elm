port module Sounds exposing (Sound(..), play)


port playPort : String -> Cmd msg


type Sound
    = OpenHat
    | ClosedHat
    | Crash
    | Stab


play : Sound -> Cmd msg
play sound =
    name sound |> playPort


name : Sound -> String
name sound =
    case sound of
        OpenHat ->
            "OH"

        ClosedHat ->
            "CH"

        Crash ->
            "crash"

        Stab ->
            "stab"
