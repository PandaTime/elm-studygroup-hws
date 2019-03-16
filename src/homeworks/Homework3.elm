module Homeworks.Homework3 exposing (
    maybeToList
  , updateList
  , find
  , updateListKv
  , keepOks
  , mapOk
  , either
  , parseDate
  )

import Date exposing (Date)
import Time
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task

{-
  maybeToList

  > maybeToList (Just 3)
  [3]
  > maybeToList Nothing
  []
-}
maybeToList : Maybe a -> List a
maybeToList x = 
  case x of
    Just q ->
      [q]
    _ ->
      []


{-
  updateList

  Change or remove element if it matches the shouldChange test.
  > updateList (\x -> x == 3) (\v -> Just (v + 1)) [1,3,5]
  [1,4,5] : List number
  > updateList (\x -> x == 3) (\v -> Nothing) [1,3,5]
  [1,5] : List number
-}
updateList : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
updateList shouldChange f xs = 
  case xs of
    [] ->
      []
    head :: rest ->
      if shouldChange head then
        case f head of
          Just v ->
            [v] ++ updateList shouldChange f rest
          Nothing ->
            updateList shouldChange f rest
      else
        [head] ++ updateList shouldChange f rest

{-
find

> find (\x -> x == 2) [1,3,5,2]
Just 2 : Maybe number
> find (\x -> x == 2) [1,3,5]
Nothing : Maybe number
-}
find : (a -> Bool) -> List a -> Maybe a
find f xss = 
  case xss of 
    head :: rest ->
      if f head then
        Just head
      else 
        find f rest
    _ ->
      Nothing

{-
Implement updateListKv

> updateListKv [("foo", 1), ("bar", 2)] "foo" (\x -> Just (x + 1))
[("foo", 2), ("bar", 2)]
> updateListKv [("foo", 1), ("bar", 2)] "foo" (\x -> Nothing)
[("bar", 2)]
-}
updateListKv :
  List (k, v)
  -> k
  -> (v -> Maybe v)
  -> List (k, v)
updateListKv old k f = 
  case old of
    head :: rest ->
      if Tuple.first head == k then
        case f (Tuple.second head) of 
          Just v ->
            [Tuple.mapSecond (\x -> v) head] ++ updateListKv rest k f
          Nothing ->
            updateListKv rest k f
      else
        [head] ++ updateListKv rest k f
    _ ->
      []

{-
  keepOks

  > keepOks [Ok 1, Err "bad", Ok 2]
  [1,2] : List number
-}

keepOks : List (Result a b) -> List b
keepOks xss = 
  case xss of
    head :: rest ->
      case head of
        Ok v ->
          [v] ++ keepOks rest
        Err _ ->
          keepOks rest
    _ ->
      []
    
{-
  mapOk

  > mapOk (\x -> x + 1) (Ok 2)
  Ok 3 : Result a number
  > mapOk (\x -> x + 1) (Err "str")
  Err "str" : Result String number
-}
mapOk : (b -> c) -> Result a b -> Result a c
mapOk f res = 
  case res of
    Ok v ->
      Ok (f v)
    Err e ->
      Err e

{-
  either

  > either (\x -> x + 1) (\x -> x - 1) (Ok 1)
  0 : number
  > either (\x -> x + 1) (\x -> x - 1) (Err 1)
  2 : number
-}
either : (a -> c) -> (b -> c) -> Result a b -> c
either fa fb res = 
  case res of
    Ok v ->
      fb v
    Err v ->
      fa v

{-
  Implement parseDate

  Do elm install justinmimbs/date to get the package. Use Date.fromIsoString.
-}
-- Maybe.andThen v ===
--   case x of 
--     Nothing -> Nothing
--     Just v -> ...
parseDate : Maybe String -> Maybe Date
parseDate v =
  case v of
    Just dateStr ->
      case Date.fromIsoString dateStr of
        Ok date ->
          Just date
        Err _ ->
          Nothing
    Nothing ->
      Nothing

{-
  Timer

  Implement "Timer" from http://eugenkiss.github.io/7guis/tasks/
-}
-- https://gist.github.com/coreytrampe/a120fac4959db7852c0f
stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            ]
        children = []
    in 
        node tag attrs children
--Main
main : Program () Model Msg
main =
  Browser.element 
      { init = init
      , update = update
      , view = view
      , subscriptions = subscriptions
    }
-- FNs
toSeconds : Int -> Float
toSeconds ms =
  toFloat (ms * 100 // second) / 100

--Model
type alias Model =
  { elapsedTimeMs : Int
  , timerDurationMs: Int}

second = 1000
timeStep = 50

init : flags -> (Model, Cmd Msg)
init flags =
  ({ elapsedTimeMs = 0
  , timerDurationMs = (10 * second) -- 10 sec
  }, Cmd.none)

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every timeStep TimeUpdate

-- UPDATE

type Msg
  = TimerDudationChange String | TimeUpdate Time.Posix | ResetTimer

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    TimerDudationChange newDuration ->
      case String.toInt newDuration of
        Just ms ->
          ({model | timerDurationMs = ms}, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    TimeUpdate time ->
      ({ model | elapsedTimeMs = (Basics.min (model.elapsedTimeMs + timeStep) model.timerDurationMs )}, Cmd.none)
    ResetTimer ->
      ({model | elapsedTimeMs = 0}, Cmd.none)


view : Model -> Html Msg
view model =
    div [] 
      [ stylesheet
      , div [class "container", class" jumbotron"] 
        [  div [] 
          [  span [] [text ("Elapsed Time:")]
            , meter [value (String.fromInt model.elapsedTimeMs), Html.Attributes.min "0", Html.Attributes.max (String.fromInt model.timerDurationMs) ] []
          ]
        , div [] 
          [ span [] []
          , div [] [ text( (String.fromFloat ( toSeconds model.timerDurationMs)) )]
        ]
        , div [] 
          [  span [] [text ("Duration:")]
          , input 
            [ type_ "range"
              , Html.Attributes.min "1"
              , Html.Attributes.max (String.fromInt(30 * second))
              , step "1"
              , value (String.fromInt model.timerDurationMs)
              , onInput TimerDudationChange
            ] []
        , div [] 
          [  button [onClick ResetTimer] [text("Reset Timer")]
          ]
        ]
      ]
    ]
