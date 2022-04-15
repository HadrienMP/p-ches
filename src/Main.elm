port module Main exposing (..)

import Browser
import Browser.Events
import ColorPalette exposing (white, yellow)
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font exposing (center)
import Element.Input exposing (button)
import Element.Region
import Html exposing (Html)
import Json.Decode exposing (succeed)
import Random
import String exposing (fromInt)
import Tempo exposing (Tempo(..), getBpm, msBetweenSixteenthNotes)
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
    { sixteenth : Int, targetStab : Maybe Int }


type State
    = On OnModel
    | Off


type alias Model =
    { state : State
    , tempo : Tempo
    }


type Msg
    = Tick Time.Posix
    | Stab
    | StartStop
    | ChangeTempo Float
    | GotTargetStab Int



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = Off, tempo = BPM 120 }, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.state, msg ) of
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

                crashCommand =
                    if on.targetStab == Just nextSixteenth then
                        play "crash"

                    else
                        Cmd.none
            in
            ( { model | state = On { on | sixteenth = nextSixteenth } }
            , Cmd.batch [ command, crashCommand ]
            )

        ( _, Stab ) ->
            ( model, play "stab" )

        ( On _, StartStop ) ->
            ( { model | state = Off }, Cmd.none )

        ( On on, GotTargetStab targetStab ) ->
            ( { model | state = On { on | targetStab = Just targetStab } }
            , Cmd.none
            )

        ( Off, StartStop ) ->
            ( { model | state = On { sixteenth = 1, targetStab = Nothing } }
            , Cmd.batch
                [ play "OH"
                , Random.generate GotTargetStab <| Random.int 1 16
                ]
            )

        ( _, ChangeTempo float ) ->
            ( { model | tempo = float |> floor |> BPM }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.state of
            On _ ->
                Time.every (toFloat <| msBetweenSixteenthNotes model.tempo) Tick

            Off ->
                Sub.none
        , Browser.Events.onKeyDown
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        case key of
                            "Enter" ->
                                succeed Stab

                            " " ->
                                succeed StartStop

                            _ ->
                                Json.Decode.fail "Nope"
                    )
            )
        ]



-- View


type alias Note =
    { sixteenth : Int
    , label : String
    , quarter : Bool
    }


view : Model -> Html Msg
view model =
    layout
        [ Element.Font.color white
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
            , tempoField model
            , startStopButton model
            ]


tempoField : Model -> Element Msg
tempoField model =
    Element.Input.slider
        [ center
        , width fill
        , behindContent
            (el
                [ width fill
                , height (px 1)
                , centerY
                , Element.Background.color white
                ]
                Element.none
            )
        ]
        { onChange = ChangeTempo
        , label = Element.Input.labelLeft [ moveUp 6 ] <| text "Tempo : "
        , min = 30
        , max = 180
        , step = Just 2
        , thumb =
            Element.Input.thumb
                [ above <| el [ centerX, moveUp 8 ] <| text <| fromInt <| getBpm <| model.tempo
                , height <| px 10
                , width <| px 10
                , Element.Border.rounded 4
                , Element.Background.color white
                ]
        , value = model.tempo |> getBpm |> toFloat
        }


startStopButton : Model -> Element Msg
startStopButton model =
    button
        [ Element.Border.solid
        , Element.Border.color white
        , Element.Border.rounded 10
        , Element.Border.width 2
        , padding 10
        ]
        { onPress = Just StartStop
        , label =
            case model.state of
                On _ ->
                    text "Stop"

                Off ->
                    text "Start"
        }


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
        , row [ centerX, spacing 70, height <| px 20 ]
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
        , row [ centerX, alignTop, spacing 70, height <| px 20 ]
            [ jojo [] { sixteenth = 10, label = "2", quarter = False }
            , jojo [] { sixteenth = 8, label = "4", quarter = False }
            ]
        , row [ centerX, height <| px 22 ]
            [ jojo [ alignBottom ] { sixteenth = 9, label = "3", quarter = True } ]
        ]


displayNote : Model -> List (Attribute Msg) -> Note -> Element Msg
displayNote model attributes note =
    el (attributes ++ commonNoteStyle model note) <|
        text note.label


commonNoteStyle : Model -> Note -> List (Attribute Msg)
commonNoteStyle model note =
    [ Element.Font.center
    , Element.Border.solid
    , Element.Border.color white
    , Element.Border.rounded 100
    ]
        ++ currentNoteStyling model note
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
    case model.state of
        On on ->
            (if note.sixteenth == on.sixteenth then
                [ Element.Background.color yellow ]

             else
                []
            )
                ++ (if Just note.sixteenth == on.targetStab then
                        [ Element.Background.color white ]

                    else
                        []
                   )

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
