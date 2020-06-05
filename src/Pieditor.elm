module Pieditor exposing (main)

import Array2 exposing (Array2)

import Browser
import Html exposing (Html, h2, input, div, text, table, tr, td, span, label)
import Html.Attributes as HAttr
import Html.Events as HEvent
import Svg exposing (svg)
import Svg.Attributes as SAttr
import Svg.Events as SEvent




-- MODEL --

type alias Model =
  { color : Color
  , canvas : Array2 Color
  , codelSize : Int
  , customColor : Bool
  }

initialModel =
  { color = White
  , canvas = Array2.repeat 32 24 White
  , codelSize = 16
  , customColor = False
  }

type Color
  = Chromatic Hue Lightness
  | Black
  | White

type alias Hue = Int
type alias Lightness = Int

hues : List Hue
hues =
  List.range 0 5

lightnesses : List Lightness
lightnesses =
  List.range 0 2

colorCode : Color -> String
colorCode color =
  case color of
    Chromatic 0 0 -> "#FFC0C0"
    Chromatic 0 1 -> "#FF0000"
    Chromatic 0 2 -> "#C00000"
    Chromatic 1 0 -> "#FFFFC0"
    Chromatic 1 1 -> "#FFFF00"
    Chromatic 1 2 -> "#C0C000"
    Chromatic 2 0 -> "#C0FFC0"
    Chromatic 2 1 -> "#00FF00"
    Chromatic 2 2 -> "#00C000"
    Chromatic 3 0 -> "#C0FFFF"
    Chromatic 3 1 -> "#00FFFF"
    Chromatic 3 2 -> "#00C0C0"
    Chromatic 4 0 -> "#C0C0FF"
    Chromatic 4 1 -> "#0000FF"
    Chromatic 4 2 -> "#0000C0"
    Chromatic 5 0 -> "#FFC0FF"
    Chromatic 5 1 -> "#FF00FF"
    Chromatic 5 2 -> "#C000C0"
    Black -> "#000000"
    White -> "#FFFFFF"
    _ -> "?"

customColorCode : Color -> String
customColorCode color =
  case color of
    Chromatic 0 0 -> "#FFA0A0"
    Chromatic 0 1 -> "#FF0000"
    Chromatic 0 2 -> "#C00000"
    Chromatic 1 0 -> "#FFFFC0"
    Chromatic 1 1 -> "#FFFF00"
    Chromatic 1 2 -> "#C0C000"
    Chromatic 2 0 -> "#A0FFA0"
    Chromatic 2 1 -> "#00FF00"
    Chromatic 2 2 -> "#00A000"
    Chromatic 3 0 -> "#C0FFFF"
    Chromatic 3 1 -> "#00FFFF"
    Chromatic 3 2 -> "#00C0C0"
    Chromatic 4 0 -> "#C0C0FF"
    Chromatic 4 1 -> "#3030FF"
    Chromatic 4 2 -> "#0000A0"
    Chromatic 5 0 -> "#FFC0FF"
    Chromatic 5 1 -> "#FF00FF"
    Chromatic 5 2 -> "#C000C0"
    Black -> "#222222"
    White -> "#FFFFFF"
    _ -> "?"



-- UPDATE --

type Msg
  = SelectColor Color
  | Plot Int Int
  | ToggleCustomColor

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    model_ =
      case msg of
        SelectColor color ->
          { model | color = color }

        Plot x y ->
          let
            canvas =
              model.canvas
                |> Array2.set x y model.color
          in
            { model | canvas = canvas }

        ToggleCustomColor ->
          { model | customColor = not model.customColor }
  in
    ( model_, Cmd.none )



-- VIEW --

view : Model -> Html Msg
view model =
  div
  []
  [ paletteView model
  , canvasView model
  ]

paletteView : Model -> Html Msg
paletteView model =
  div
  []
  [ h2 [] [ text <| "palette" ]
  , table []
    [ tr [] (colorButtons 0 model)
    , tr [] (colorButtons 1 model)
    , tr [] (colorButtons 2 model)
    , tr []
      [ colorTd Black model
      , colorTd White model
      ]
    ]
  , label []
    [ input
      [ HAttr.type_ "checkbox"
      , HEvent.onClick ToggleCustomColor ] []
    , span [] [ text "custom color" ]
    ]
  ]

colorButtons : Lightness -> Model -> List (Html Msg)
colorButtons lightness model =
  hues
    |> List.map (\h ->
        colorTd (Chromatic h lightness) model )

colorTd : Color -> Model -> Html Msg
colorTd color model =
  input
  [ HAttr.type_ <| "button"
  , HEvent.onClick <| SelectColor color
  , HAttr.style "background-color" <|
      if model.customColor
      then customColorCode <| color
      else colorCode <| color
  , HAttr.style "border-style" <|
      if color == model.color
      then "inset"
      else "outset"
  ]
  []
    |> List.singleton
    |> td []

canvasView : Model -> Html Msg
canvasView model =
  let
    size = model.codelSize
    w = Array2.width  model.canvas * size |> String.fromInt
    h = Array2.height model.canvas * size |> String.fromInt
  in
    div []
    [ h2 [] [ text <| "canvas" ]
    , svg
      [ SAttr.width w, SAttr.height h, SAttr.viewBox ("0 0 "++w++" "++h) ]
      (model.canvas
        |> Array2.toIndexedList
        |> List.map (\( ( x, y ), c ) ->
          Svg.rect
          [ SAttr.x        <| String.fromInt <| x * size
          , SAttr.y        <| String.fromInt <| y * size
          , SAttr.width    <| String.fromInt <| size
          , SAttr.height   <| String.fromInt <| size
          , SAttr.stroke   <| "#CCCCCC"
          , SAttr.fill     <|
              if model.customColor
              then customColorCode <| c
              else colorCode       <| c
          , SEvent.onClick <| Plot x y
          ]
          []))
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