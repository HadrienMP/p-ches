port module Main exposing (..)

import Browser
import Browser.Events
import ColorPalette exposing (white, yellow)
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input exposing (button)
import Element.Region
import Html exposing (Html)
import Json.Decode
import Tempo exposing (Tempo(..), getBpm, msBetweenQuarterNotes)
import Time


port play : String -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias OnModel =
    { sixteenth : Int }


type Model
    = On OnModel
    | Off


type Msg
    = Tick Time.Posix
    | Stab
    | Stop
    | Start



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( Off, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( On on, Tick _ ) ->
            let
                nextSixteenth =
                    modBy 16 on.sixteenth + 1

                isQuarter =
                    modBy 4 nextSixteenth == 1

                isFirstQuarter =
                    nextSixteenth == 1

                command =
                    if isFirstQuarter then
                        play "OH"

                    else if isQuarter then
                        play "CH"

                    else
                        Cmd.none
            in
            ( On { on | sixteenth = nextSixteenth }, command )

        ( On _, Stab ) ->
            ( model, play "stab" )

        ( On _, Stop ) ->
            ( Off, Cmd.none )

        ( Off, Start ) ->
            ( On { sixteenth = 1 }, play "OH" )

        _ ->
            ( model, Cmd.none )



-- Subscriptions


tempo : Tempo
tempo =
    BPM 120


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (toFloat <| msBetweenQuarterNotes tempo) Tick
        , Browser.Events.onKeyDown (Json.Decode.succeed Stab)
        ]



-- View


type alias Note =
    { sixteenth : Int
    , label : String
    , quarter : Bool
    }


notes : List Note
notes =
    List.range 1 16
        |> List.map
            (\sixteenth ->
                { sixteenth = sixteenth
                , label =
                    if modBy 4 sixteenth == 1 then
                        sixteenth // 4 |> (+) 1 |> String.fromInt

                    else
                        modBy 4 (sixteenth - 1) + 1 |> String.fromInt
                , quarter = modBy 4 sixteenth == 1
                }
            )


view : Model -> Html Msg
view model =
    layout
        [ Element.Font.color white
        , clipX
        , Element.Font.family
            [ Element.Font.external
                { url = "https://fonts.googleapis.com/css2?family=Permanent+Marker&display=swap"
                , name = "Permanent Marker"
                }
            , Element.Font.sansSerif
            ]
        ]
    <|
        column [ centerY, centerX, spacing 60, paddingXY 10 28 ]
            [ displayNotes model
            , playStopButon model
            ]


playStopButon : Model -> Element Msg
playStopButon model =
    row [ centerX, spacingXY 40 0 ]
        [ button
            [ Element.Border.solid
            , Element.Border.color white
            , Element.Border.rounded 10
            , Element.Border.width 2
            , padding 10
            ]
            { onPress =
                case model of
                    On _ ->
                        Just Stop

                    Off ->
                        Just Start
            , label =
                case model of
                    On _ ->
                        text "Stop"

                    Off ->
                        text "Start"
            }
        , text <| "Tempo: " ++ String.fromInt (getBpm tempo)
        ]


displayNotes : Model -> Element Msg
displayNotes model =
    let
        jojo =
            displayNote model
    in
    column
        [ centerX
        , Element.Font.family [ Element.Font.sansSerif ]
        , inFront <|
            button [ centerX, centerY ]
                { label =
                    image [ width <| px 100 ]
                        { src = "/img/peach.png", description = "peach" }
                , onPress = Just Stab
                }
        ]
        [ row [ centerX, height <| px 22 ]
            [ jojo [ alignTop ] { sixteenth = 1, label = "1", quarter = True } ]
        , row [ centerX, spacing 66, height <| px 20 ]
            [ jojo [] { sixteenth = 16, label = "4", quarter = False }
            , jojo [] { sixteenth = 2, label = "2", quarter = False }
            ]
        , row [ centerX, spacing 138 ]
            [ jojo [] { sixteenth = 15, label = "3", quarter = False }
            , jojo [] { sixteenth = 3, label = "3", quarter = False }
            ]
        , row [ centerX, spacing 188, height <| px 44 ]
            [ jojo [] { sixteenth = 14, label = "2", quarter = False }
            , jojo [] { sixteenth = 4, label = "4", quarter = False }
            ]
        , row [ centerX, spacing 194, height <| px 56 ]
            [ jojo [] { sixteenth = 13, label = "4", quarter = True }
            , jojo [] { sixteenth = 5, label = "2", quarter = True }
            ]
        , row [ centerX, spacing 188, height <| px 44 ]
            [ jojo [] { sixteenth = 12, label = "4", quarter = False }
            , jojo [] { sixteenth = 6, label = "2", quarter = False }
            ]
        , row [ centerX, spacing 138 ]
            [ jojo [] { sixteenth = 11, label = "3", quarter = False }
            , jojo [] { sixteenth = 7, label = "3", quarter = False }
            ]
        , row [ centerX, alignTop, spacing 66, height <| px 20 ]
            [ jojo [] { sixteenth = 10, label = "2", quarter = False }
            , jojo [] { sixteenth = 8, label = "4", quarter = False }
            ]
        , row [ centerX, height <| px 22 ]
            [ jojo [ alignBottom ] { sixteenth = 9, label = "3", quarter = True } ]
        ]


displayNote : Model -> List (Attribute Msg) -> Note -> Element Msg
displayNote model attributes note =
    el (attributes ++ commonNoteStyle note ++ currentNoteStyling model note) <|
        text note.label


commonNoteStyle : Note -> List (Attribute msg)
commonNoteStyle note =
    [ Element.Font.center
    , Element.Border.solid
    , Element.Border.color white
    , Element.Border.rounded 100
    ]
        ++ (case modBy 4 (note.sixteenth - 1) of
                0 ->
                    [ Element.Border.width 4
                    , paddingEach { top = 10, left = 13, right = 13, bottom = 8 }
                    , Element.Font.size 20
                    , Element.Font.bold
                    ]

                2 ->
                    [ Element.Border.width 2
                    , paddingXY 10 8
                    , Element.Font.size 14
                    , Element.Font.bold
                    ]

                _ ->
                    [ Element.Border.width 1
                    , paddingXY 10 8
                    , Element.Font.size 10
                    ]
           )


currentNoteStyling : Model -> Note -> List (Attribute Msg)
currentNoteStyling model note =
    case model of
        On on ->
            [ if note.sixteenth == on.sixteenth then
                Element.Background.color yellow

              else
                Element.Font.center
            ]

        Off ->
            []


title : Element.Element Msg
title =
    row
        [ centerX
        , width shrink
        ]
        [ image [ width <| px 100 ] { src = "/img/peach.png", description = "peach" }
        , el
            [ Element.Font.bold
            , Element.Region.heading 1
            , Element.Font.size 60
            , alignBottom
            , moveUp 30
            , rotate <| degrees -10
            ]
          <|
            text "Pêches"
        ]
