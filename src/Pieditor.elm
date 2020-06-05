module Pieditor exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MODEL --

type alias Model =
  { count : Int }

initialModel =
  { count = 0 }



-- UPDATE --

type Msg
  = Increment
  | Decrement

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Increment ->
      ( { model | count = model.count + 1 }
      , Cmd.none
      )

    Decrement ->
      ( { model | count = model.count - 1 }
      , Cmd.none
      )



-- VIEW --

view : Model -> Html Msg
view model =
  div
  []
  [ button
    [ onClick Increment ]
    [ text "+1" ]
  , div
    []
    [ text <| String.fromInt model.count ]
  , button
    [ onClick Decrement ]
    [ text "-1" ]
  ]



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- MAIN --

main =
  Browser.element
  { init = \() -> ( initialModel, Cmd.none )
  , view = view
  , update = update
  , subscriptions = subscriptions
  }