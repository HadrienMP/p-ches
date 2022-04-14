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
        , Browser.Events.onClick (Json.Decode.succeed Stab)
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
            [ title
            , row [ centerX, spacingXY 40 0 ]
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
            , displayNotes model
            ]


displayNotes : Model -> Element Msg
displayNotes model =
    row [ spacing 4, width fill, centerX, Element.Font.family [ Element.Font.sansSerif ] ]
        (notes |> List.map (displayNote model))


displayNote : Model -> Note -> Element Msg
displayNote model note =
    case model of
        On on ->
            el
                [ Element.Font.center
                , Element.Border.solid
                , Element.Border.color white
                , Element.Border.rounded 10
                , if note.quarter then
                    Element.Border.width 2

                  else
                    Element.Border.width 1
                , if note.sixteenth == on.sixteenth then
                    Element.Background.color yellow

                  else
                    Element.Font.center
                , if note.quarter then
                    paddingEach { top = 10, left = 8, right = 8, bottom = 8 }

                  else
                    padding 8
                , if note.quarter then
                    Element.Font.size 20

                  else
                    Element.Font.size 10
                ]
            <|
                text note.label

        Off ->
            el
                [ Element.Font.center
                , Element.Border.solid
                , Element.Border.color white
                , if note.quarter then
                    Element.Border.width 2

                  else
                    Element.Border.width 1
                , Element.Border.rounded 10
                , if note.quarter then
                    paddingEach { top = 10, left = 8, right = 8, bottom = 8 }

                  else
                    padding 8
                , if note.quarter then
                    Element.Font.size 20

                  else
                    Element.Font.size 10
                ]
            <|
                text note.label


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
