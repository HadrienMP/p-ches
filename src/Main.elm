module Main exposing (..)

import Browser
import Html exposing (Html, text)


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
    text "Peaches"
