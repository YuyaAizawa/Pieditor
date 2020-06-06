module Pieditor exposing (main)

import Array2 exposing (Array2)

import Browser
import Html exposing (Html, h2, input, div, text, table, tr, td, span, label)
import Html.Attributes as HAttr
import Html.Events as HEvent
import Set
import Svg exposing (svg)
import Svg.Attributes as SAttr
import Svg.Events as SEvent




-- MODEL --

type alias Model =
  { tool : Tool
  , color : Color
  , canvas : Array2 Color
  , codelSize : Int
  , customColor : Bool
  }

initialModel =
  { tool = Pencil
  , color = White
  , canvas = Array2.repeat 32 24 White
  , codelSize = 16
  , customColor = False
  }


type Tool
  = Pencil
  | Paint

toolIcon : Tool -> String
toolIcon tool =
  case tool of
    Pencil -> "pencil"
    Paint  -> "paint"


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
  = ChangeTool Tool
  | SelectColor Color
  | Plot Int Int
  | ToggleCustomColor

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    model_ =
      case msg of
        ChangeTool tool ->
          { model | tool = tool }

        SelectColor color ->
          { model | color = color }

        Plot x y ->
          case model.tool of
            Pencil ->
              let
                canvas =
                  model.canvas
                    |> Array2.set x y model.color
              in
                { model | canvas = canvas }

            Paint ->
              let
                canvas =
                  model.canvas
                    |> paint x y model.color
              in
                { model | canvas = canvas }

        ToggleCustomColor ->
          { model | customColor = not model.customColor }
  in
    ( model_, Cmd.none )

paint : Int -> Int -> Color -> Array2 Color -> Array2 Color
paint x y color canvas =
  let
    area =
      Array2.connectedArea x y canvas
  in
    Set.foldl
    (\( x_, y_ ) -> Array2.set x_ y_ color)
    canvas
    area


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
  let
    colorRow lightness =
      hues
        |> List.map (\h ->
          colorGrid (Chromatic h lightness) model )
  in
    div
    [ HAttr.id "palette" ]
    [ h2 [] [ text <| "palette" ]
    , [ colorRow 0
      , colorRow 1
      , colorRow 2
      , [ colorGrid Black model
        , colorGrid White model
        , toolGrid Pencil model
        , toolGrid Paint model
        ]
      ] |> grid 6 4
    , label []
      [ input
        [ HAttr.type_ "checkbox"
        , HEvent.onClick ToggleCustomColor ] []
      , span [] [ text "custom color" ]
      ]
    ]

colorGrid : Color -> Model -> Html Msg
colorGrid color model =
  input
  [ HAttr.type_ <| "button"
  , HAttr.style "background-color" <|
      if model.customColor
      then customColorCode <| color
      else colorCode <| color
  , HEvent.onClick <| SelectColor color
  , HAttr.style "border-style" <|
      if color == model.color
      then "inset"
      else "outset"
  ][]

toolGrid : Tool -> Model -> Html Msg
toolGrid tool model =
  input
  [ HAttr.type_ "button"
  , HAttr.value <| toolIcon <| tool
  , HEvent.onClick <| ChangeTool tool
  , HAttr.class "lsf tool"
  , HAttr.style "border-style" <|
      if tool == model.tool
      then "inset"
      else "outset"
  ][]

grid : Int -> Int -> List (List (Html Msg)) -> Html Msg
grid columns rows contents =
  div
  [ HAttr.class "grid" ]
  (contents
    |> List.indexedMap (\i row -> row
      |> List.indexedMap (\j column ->
        div
        [ HAttr.style "grid-row"    <| String.fromInt <| i+1
        , HAttr.style "grid-column" <| String.fromInt <| j+1
        ]
        [ column ]))
    |> List.concat
  )

canvasView : Model -> Html Msg
canvasView model =
  let
    size = model.codelSize
    w = Array2.width  model.canvas * size |> String.fromInt
    h = Array2.height model.canvas * size |> String.fromInt
  in
    div
    [ HAttr.id "canvas" ]
    [ h2 [] [ text <| "canvas" ]
    , svg
      [ SAttr.width w, SAttr.height h, SAttr.viewBox ("0 0 "++w++" "++h) ]
      (model.canvas
        |> Array2.toListUsingIndex (\x y c ->
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