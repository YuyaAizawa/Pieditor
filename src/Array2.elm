module Array2 exposing
  ( Array2
  , empty
  , isEmpty
  , width
  , height
  , initialize
  , repeat
  , get
  , set
  , toList
  , toListUsingIndex
  , toListByRow
  , map
  , indexedMap
  , fold
  , connectedArea
  )

import Array exposing (Array)
import Set exposing (Set)

type Array2 a = Array2 Int Int (Array a)

empty : Array2 a
empty =
  Array2 0 0 Array.empty

contents : Array2 a -> Array a
contents (Array2 _ _ contents_) =
  contents_

isEmpty : Array2 a -> Bool
isEmpty this =
  this
    |> contents
    |> Array.isEmpty

width : Array2 a -> Int
width (Array2 width_ _ _) =
  width_

height : Array2 a -> Int
height (Array2 _ height_ _) =
  height_

indexMapper : (Int -> Int -> f) -> Array2 a -> (Int -> f)
indexMapper fn (Array2 width_ height_ _) =
  \i -> fn (modBy width_ i) ((//) i width_)

initialize : Int -> Int -> (Int -> Int -> a) -> Array2 a
initialize width_ height_ fn =
  let
    dummy = Array2 width_ height_ Array.empty
    fn_ = dummy |> indexMapper fn
  in
    Array.initialize (width_ * height_) fn_
      |> Array2 width_ height_

repeat : Int -> Int -> a -> Array2 a
repeat width_ height_ e =
  initialize width_ height_ (\_ _ -> e)

rangeCheck : Int -> Int -> Array2 a -> Bool
rangeCheck x y this =
  0 <= x &&
  x < (this |> width) &&
  0 <= y &&
  y < (this |> height)

get : Int -> Int -> Array2 a -> Maybe a
get x y this =
  if this |> rangeCheck x y
  then
    this
      |> contents
      |> Array.get (y * (this |> width) + x)
  else
    Nothing

set : Int -> Int -> a -> Array2 a -> Array2 a
set x y a ((Array2 width_ height_ contents_) as this) =
  if this |> rangeCheck x y
  then
    contents_
      |> Array.set (y * width_ + x) a
      |> Array2 width_ height_
  else
    this

toList : Array2 a -> List a
toList this =
  this
    |> contents
    |> Array.toList

toListUsingIndex : (Int -> Int -> a -> b) ->Array2 a -> List b
toListUsingIndex f (Array2 width_ height_ contents_) =
  contents_
    |> Array.toIndexedList
    |> List.map (\( i, a ) ->
      f (modBy width_ i) ((//) i width_) a
    )

toListByRow : Array2 a -> List (List a)
toListByRow (Array2 width_ height_ contents_) =
  List.range 0 (height_ - 1)
    |> List.map (\row ->
      contents_
        |> Array.slice (row * width_) ((row + 1) * width_)
        |> Array.toList
    )

indexedMap : (Int -> Int -> a -> b) -> Array2 a -> Array2 b
indexedMap fn ((Array2 width_ height_ contents_) as this) =
  contents_
    |> Array.indexedMap (this |> indexMapper fn)
    |> Array2 width_ height_

map : (a -> b) -> Array2 a -> Array2 b
map fn =
  indexedMap (\_ _ a -> fn a)

fold : (a -> b -> b) -> b -> Array2 a -> b
fold fn acc this =
  this
    |> contents
    |> Array.foldl fn acc

connectedArea : Int -> Int -> Array2 a -> Set ( Int, Int )
connectedArea x y this =
  let
    target = this |> get x y

    neighbors ( x_, y_ ) =
      [ ( x_ + 1, y_ ), ( x_ - 1, y_ )
      , ( x_, y_ + 1 ), ( x_, y_ - 1 )]

    help : List ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
    help candidates result =
      case candidates of
        [] ->
          result

        (( x_, y_ ) as hd) :: tl ->
          if (Set.member hd result |> not) && get x_ y_ this == target
          then help (neighbors hd ++ tl) (Set.insert hd result)
          else help tl result
  in
    case target of
      Nothing -> Set.empty
      Just a -> help [ ( x, y ) ] Set.empty