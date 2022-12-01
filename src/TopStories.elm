module TopStories exposing (main)

import Browser
import Html exposing (..)


type alias Model =
    {}


type Msg
    = Dummy


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "hello" ]
