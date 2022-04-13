module Main exposing (..)

import Browser
import Element exposing (Element, alignBottom, centerX, centerY, column, el, fill, htmlAttribute, image, layout, moveLeft, moveRight, moveUp, none, padding, paddingEach, paddingXY, px, rgb, rgb255, rotate, row, shrink, spaceEvenly, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Region exposing (description)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { sixteenth : Int }


type Msg
    = Tick Time.Posix



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sixteenth = 1 }, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | sixteenth = modBy 16 model.sixteenth + 1 }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 500 Tick



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
        [ htmlAttribute <| style "background-color" "#4158D0"
        , htmlAttribute <| style "background-image" "linear-gradient(43deg, #4158D0 0%, #C850C0 46%, #FFCC70 100%)"
        , Element.Font.color white
        ]
    <|
        column [ centerY, centerX, spacing 100 ]
            [ title
            , row [ spaceEvenly, width fill, centerX ]
                (notes |> List.map (displayNote model))
            ]


displayNote : Model -> Note -> Element Msg
displayNote model note =
    el
        [ Element.Font.center
        , Element.Border.solid
        , Element.Border.color white
        , if note.quarter then
            Element.Border.width 2

          else
            Element.Border.width 1
        , Element.Border.rounded 10
        , if note.sixteenth == model.sixteenth then
            Element.Background.color yellow

          else
            Element.Font.center
        , paddingEach { top = 10, left = 8, right = 8, bottom = 8 }
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
        [ image [ width <| px 180 ] { src = "/img/peach.png", description = "peach" }
        , el
            [ Element.Font.bold
            , Element.Region.heading 1
            , Element.Font.size 100
            , Element.Font.family
                [ Element.Font.external { url = "https://fonts.googleapis.com/css2?family=Permanent+Marker&display=swap", name = "Permanent Marker" }
                , Element.Font.sansSerif
                ]
            , alignBottom
            , moveUp 50
            , rotate <| degrees -10
            ]
          <|
            text "Pêches"
        ]


white : Element.Color
white =
    rgb255 255 255 255


yellow : Element.Color
yellow =
    rgb255 255 203 113
