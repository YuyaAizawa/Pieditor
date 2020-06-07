module Pieditor exposing (main)

import Array2 exposing (Array2)

import Browser
import Html exposing (Html, h2, input, div, text, table, tr, td, span, label, br)
import Html.Attributes as HAttr
import Html.Events as HEvent
import Set
import Svg exposing (Svg, svg)
import Svg.Attributes as SAttr
import Svg.Events as SEvent




-- MODEL --

type alias Model =
  { tool : Tool
  , color : Color
  , canvas : Canvas
  , codelSize : Int
  , customColor : Bool
  , vm : Vm
  }

initialModel =
  { tool = Pencil
  , color = White
  , canvas = Array2.repeat 32 24 White
  , codelSize = 16
  , customColor = False
  , vm = initialVm
  }

updateVm : (Vm -> Vm) -> Model -> Model
updateVm updater model =
  { model | vm = updater model.vm }


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

type alias Canvas = Array2 Color

type alias Vm =
  { pc : ( Int, Int )
  , dp : Dp
  , cc : Cc
  , prevDp : Maybe Dp
  }

initialVm =
  { pc = ( 0, 0 )
  , dp = DpRight
  , cc = CcLeft
  , prevDp = Nothing
  }

type Dp
  = DpRight
  | DpDown
  | DpLeft
  | DpUp

dpToString : Dp -> String
dpToString dp =
  case dp of
    DpRight -> "Right"
    DpDown  -> "Down"
    DpLeft  -> "Left"
    DpUp    -> "Up"

type Cc
  = CcLeft
  | CcRight

ccToString : Cc -> String
ccToString cc =
  case cc of
    CcRight -> "Right"
    CcLeft  -> "Left"



-- UPDATE --

type Msg
  = ChangeTool Tool
  | SelectColor Color
  | Plot Int Int
  | ToggleCustomColor
  | Step
  | Reset

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

        Step ->
          let
            (( nextX, nextY ) as coord) =
              model.vm
                |> findsEdges model.canvas
                |> furthestToTheCc model.vm
                |> Maybe.withDefault ( 0, 0 )
                |> travelsInToDp model

            nextState =
              case model.canvas |> Array2.get nextX nextY of
                Nothing ->
                  switchDirection model.vm
                Just Black ->
                  switchDirection model.vm

                _ ->
                  chgangePc coord model.vm
          in
            { model | vm = nextState }

        Reset ->
          { model | vm = initialVm }
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

findsEdges : Canvas -> Vm -> List ( Int, Int )
findsEdges canvas vm =
  let
    ( x, y ) = vm.pc

    order =
      case vm.dp of
        DpRight ->
          \( x1, y1 ) ( x2, y2 ) -> compare x2 x1
        DpDown ->
          \( x1, y1 ) ( x2, y2 ) -> compare y2 y1
        DpLeft ->
          \( x1, y1 ) ( x2, y2 ) -> compare x1 x2
        DpUp ->
          \( x1, y1 ) ( x2, y2 ) -> compare y1 y2
  in
    canvas
      |> Array2.connectedArea x y
      |> Set.foldl
        (\coord (tentative, list) ->
          case list of
            [] ->
              ( coord, [ coord ] )

            _ -> case order coord tentative of
              LT -> ( coord, [ coord ] )
              EQ -> ( coord, coord :: list )
              GT -> ( tentative, list )
        )
        ( ( 0, 0 ), [] )
      |> Tuple.second

furthestToTheCc : Vm -> List ( Int, Int ) -> Maybe ( Int, Int )
furthestToTheCc vm candidates =
  case ( vm.dp, vm.cc ) of
    ( DpRight, CcLeft  ) -> candidates |> minBy Tuple.second
    ( DpLeft , CcRight ) -> candidates |> minBy Tuple.second
    ( DpDown , CcLeft  ) -> candidates |> maxBy Tuple.first
    ( DpUp   , CcRight ) -> candidates |> maxBy Tuple.first
    ( DpRight, CcRight ) -> candidates |> maxBy Tuple.second
    ( DpLeft , CcLeft  ) -> candidates |> maxBy Tuple.second
    ( DpDown , CcRight ) -> candidates |> minBy Tuple.first
    ( DpUp   , CcLeft  ) -> candidates |> minBy Tuple.first

travelsInToDp : Model -> ( Int, Int ) -> ( Int, Int )
travelsInToDp model ( x, y ) =
  case model.vm.dp of
    DpRight -> ( x + 1, y     )
    DpDown  -> ( x    , y + 1 )
    DpLeft  -> ( x - 1, y     )
    DpUp    -> ( x    , y - 1 )

switchDirection vm =
  if vm.prevDp == Just vm.dp
  then stepDp vm
  else stepCc vm

chgangePc coord vm =
  { vm
  | pc = coord
  , prevDp = Nothing
  }

stepDp vm =
  let
    nextDp = case vm.dp of
      DpRight -> DpDown
      DpDown  -> DpLeft
      DpLeft  -> DpUp
      DpUp    -> DpRight
  in
    { vm
    | dp = nextDp
    , prevDp = Just vm.dp
    }

stepCc vm =
  let
    nextCc = case vm.cc of
      CcLeft  -> CcRight
      CcRight -> CcLeft
  in
    { vm
    | cc = nextCc
    , prevDp = Just vm.dp
    }



-- VIEW --

view : Model -> Html Msg
view model =
  div
  []
  [ paletteView model
  , vmView model
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


vmView : Model -> Html Msg
vmView model =
  div
  [ HAttr.id "controler" ]
  [ h2 [] [ text <| "controler" ]
  , vmStateView model.vm
  , input
    [ HAttr.type_ "button"
    , HEvent.onClick Step
    , HAttr.value "step"
    ][]
  , input
    [ HAttr.type_ "button"
    , HEvent.onClick Reset
    , HAttr.value "reset"
    ][]
  ]

vmStateView : Vm -> Html msg
vmStateView vm =
  let

    ( x, y ) = vm.pc

    pcStr =
      "pc: (" ++ String.fromInt x ++
      ", " ++ String.fromInt y ++ ")"

    dpStr =
      "dp: " ++ dpToString vm.dp

    ccStr =
      "cc: " ++ ccToString vm.cc
  in
    div
    []
    [ text <| pcStr, br[][]
    , text <| dpStr, br[][]
    , text <| ccStr
    ]


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
      ((model.canvas
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
          [])) ++ [ pcSvg size model.vm.pc ]
      )
    ]

pcSvg : Int -> ( Int, Int ) -> Svg msg
pcSvg size ( x, y ) =
  Svg.rect
    [ SAttr.x        <| String.fromInt <| x * size + (size // 3)
    , SAttr.y        <| String.fromInt <| y * size + (size // 3)
    , SAttr.width    <| String.fromInt <| size // 3
    , SAttr.height   <| String.fromInt <| size // 3
    , SAttr.stroke   <| "#CCCCCC"
    , SAttr.fill     <| "none"
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



-- UTILITY --

minBy : (a -> number) -> List a -> Maybe a
minBy extractor target =
  List.foldl
  (\a mab ->
    case mab of
      Nothing ->
        Just ( a, extractor a )

      Just ( a_, b_ ) ->
        let
          b = extractor a
        in
          if b < b_
          then Just ( a, b )
          else mab)
  Nothing
  target
    |> Maybe.map Tuple.first

maxBy : (a -> number) -> List a -> Maybe a
maxBy extractor target =
  List.foldl
  (\a mab ->
    case mab of
      Nothing ->
        Just ( a, extractor a )

      Just ( a_, b_ ) ->
        let
          b = extractor a
        in
          if b > b_
          then Just ( a, b )
          else mab)
  Nothing
  target
    |> Maybe.map Tuple.first
