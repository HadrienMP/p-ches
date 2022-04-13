module Main exposing (..)

import Browser
import Element exposing (alignBottom, centerX, centerY, column, el, fill, htmlAttribute, image, layout, moveLeft, moveRight, moveUp, none, padding, paddingEach, paddingXY, px, rgb, rotate, row, shrink, spaceEvenly, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Region exposing (description)
import Html exposing (Html)
import Html.Attributes exposing (style)


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
    {}


type Msg
    = Noop



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


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
                [ division "1"
                , subdivision
                , subdivision
                , subdivision
                , division "2"
                , subdivision
                , subdivision
                , subdivision
                , division "3"
                , subdivision
                , subdivision
                , subdivision
                , division "4"
                , subdivision
                , subdivision
                , subdivision
                ]
            ]


division : String -> Element.Element Msg
division s =
    el
        [ Element.Font.center
        , Element.Border.solid
        , Element.Border.color white
        , Element.Border.width 2
        , Element.Border.rounded 10
        , paddingEach { top = 10, left = 8, right = 8, bottom = 8 }
        ]
    <|
        text s


subdivision : Element.Element Msg
subdivision =
    el
        [ Element.Font.center
        , Element.Border.solid
        , Element.Border.color white
        , Element.Border.width 1
        , Element.Border.rounded 10
        , paddingEach { top = 10, left = 8, right = 8, bottom = 8 }
        ]
        none


title : Element.Element Msg
title =
    row
        [ centerX
        , width shrink
        ]
        [ image [ width <| px 160 ] { src = "/img/peach.png", description = "peach" }
        , el
            [ Element.Font.bold
            , Element.Font.size 80
            , Element.Font.family
                [ Element.Font.external { url = "https://fonts.googleapis.com/css2?family=Rock+Salt&display=swap", name = "Rock Salt" }
                , Element.Font.sansSerif
                ]
            , alignBottom
            , moveUp 50
            , moveRight 20
            , rotate <| degrees -10
            ]
          <|
            text "Pêches"
        ]


white : Element.Color
white =
    rgb 255 255 255
