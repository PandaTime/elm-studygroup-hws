module Homeworks.Homework2 exposing (
    convert
  , convert02
  , convert03
  , bird
  , bird2
  , bird3
  , setPhone
  , mapMaybes
  , catMaybes
  , buildStatsUrl
  , toFahrenheit
  , toCelsius
  , queens
  )
  
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Url.Builder
import Html.Events exposing (onInput)

{-
Map one structure to another

convert 
  : List { name : String, email : String, phone_number : String}
  -> List { name : String, email : String}
convert = Debug.todo ""
> convert [{name="John", email="john@gmail.com", phone_number="+3801234567"}]
[{name="John", email="john@gmail.com"}]
-}

type alias InputConvertRecord = { name : String, email : String, phone_number : String}

type alias OutputConvertRecord = { name : String, email : String}

convert : List InputConvertRecord -> List OutputConvertRecord
convert a =
  let 
    convertRecord : InputConvertRecord -> OutputConvertRecord
    convertRecord record = { name = record.name, email = record.email }
  in
    List.map convertRecord a


{- 
Filter elements with non-empty name and email

convert02 
  : List { name : Maybe String, email : Maybe String} 
  -> List { name : String, email : String} 
convert02 = Debug.todo ""
> convert02 [{name=Just "John", email=Just "john@gmail.com"}]
[{name="John", email="john@gmail.com"}]
-}

type alias InputConvert02Record = { name : Maybe String, email : Maybe String}

type alias OutputConvert02Record = { name : String, email : String}

convert02 : List InputConvert02Record -> List OutputConvert02Record
convert02 a = 
  let
    getValueKey : InputConvert02Record -> Maybe OutputConvert02Record
    getValueKey {name, email} = 
      case (name, email) of
        (Just n, Just m) ->
          Just { name = n, email = m }
        (_, _) ->
          Nothing
  in
    List.filterMap getValueKey a

{-
Fill in missing emails with <unspecified>, while removing elements with no name

convert03 
  : List { name : Maybe String, email : Maybe String} 
  -> List { name : String, email : String} 
convert03 = Debug.todo ""
> convert03 [{name=Just "John", email=Nothing}]
[{name="John", email="<unspecified>"}]
-}
convert03 : List InputConvert02Record -> List OutputConvert02Record
convert03 a =
  let
    getValueKey : InputConvert02Record -> Maybe OutputConvert02Record
    getValueKey {name, email} = 
      case (name, email) of
        (Just n, Just m) ->
          Just { name = n, email = m }
        (Just n, m) -> 
          Just {name = n, email = "<unspecified>"}
        (_, _) ->
          Nothing
  in
    List.filterMap getValueKey a

{-
  Rewrite bird using <|, then using |> instead of parens (where applicable)
  bird : Int
  bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
      List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))
-}
bird : Int
bird =
  let
    notThree x =
      x /= 3

    incr x =
      x + 1
  in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))
-- using <|
bird2 : Int
bird2 =
  let
    notThree x =
      x /= 3

    incr x =
      x + 1
  in 
    List.sum <| List.filter notThree <| List.map incr <| [ 1, 2, 3 ]

-- using |>
bird3 : Int
bird3 = 
  let
    notThree x =
      x /= 3

    incr x =
      x + 1
  in 
    [ 1, 2, 3 ] |> List.map incr |> List.filter notThree |> List.sum

{-
Implement setPhone

setPhone : String -> User -> User
setPhone = Debug.todo ""

> setPhone "+123456" { profile = { address = { phone = "+654321" } } }
{ profile = { address = { phone = "+123456" } } }
-}
type alias User = { profile : Profile }
type alias Profile = { address : Address }
type alias Address = { phone : String }

setPhone : String -> User -> User
setPhone number user = 
  let
    setUserPhone (u, phone) = 
      { u | profile = updateProfilePhone (u.profile, phone) }

    updateProfilePhone (profile, phone) =
      { profile | address = updateAddressPhone (profile.address, phone) }

    updateAddressPhone (address, phone) =
      { address | phone = phone}
  in
    setUserPhone (user, number)

{-
mapMaybes
> mapMaybes (\x -> if x == Just 3 then x else Just 4) [Just 1, Nothing, Just 3]
[4,4,3] : List number
-}
mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes fn list =
  case list of
    [] ->
      []
    head :: rest ->
      case fn head of
        Nothing ->
          mapMaybes fn rest
        Just a ->
          [a] ++ mapMaybes fn rest

{-
catMaybes
> catMaybes [Just 1, Nothing, Just 3]
[1,3] : List number
-}
catMaybes : List (Maybe a) -> List a
catMaybes list =
  case list of
    [] ->
      []
    head :: rest ->
      case head of
        Nothing ->
          catMaybes rest
        Just a ->
          [a] ++ catMaybes rest

{-
Use package elm/url and its Url.Builder.absolute to build URL from parameters

> buildStatsUrl 12 {startDate=Nothing, numElems=Nothing}
https://myapi.com/api/item/12/stats.json
> buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Nothing}
https://myapi.com/api/item/12/stats.json?start_date=2019-01-01
> buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Just 10}
https://myapi.com/api/item/12/stats.json?start_date=2019-01-01&num_items=10
-}
buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buildStatsUrl itemId ps =
  let 
    itemIdString = String.fromInt itemId
    
    maybeQueryParam : (String -> a -> Url.Builder.QueryParameter) -> String -> Maybe a -> Maybe Url.Builder.QueryParameter
    maybeQueryParam paramType queryName queryParam =
      case queryParam of
        Nothing -> 
          Nothing
        Just param -> 
          Just (paramType queryName param)
  
    { startDate, numElems } = ps
    queryParams = [
        maybeQueryParam Url.Builder.string "start_date" startDate
      , maybeQueryParam Url.Builder.int "num_elems" numElems
      ]
  in
    Url.Builder.crossOrigin "https://myapi.com" ["api", "item",  itemIdString, "stats.json"] (catMaybes queryParams)

{-
Temperature converter

Implement "Temperature Converter" from 7GYUs as described in https://eugenkiss.github.io/7guis/tasks
-}
toFahrenheit : Float -> Float
toFahrenheit c = 
  c * (9/5) + 32

toCelsius : Float -> Float
toCelsius f =
  (f - 32) * 5/9
-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { celsium : Float
  , fahrenheit: Float }

init : Model
init =
  { celsium = 0.0
  , fahrenheit = toFahrenheit 0.0
  }

-- UPDATE

type Msg
  = CelsiumChange String | FahrenheitChange String


update : Msg -> Model -> Model
update msg model =
  case msg of
    CelsiumChange newCelsium ->
      case String.toFloat newCelsium of
        Just c ->
          { model | celsium = c, fahrenheit = toFahrenheit c }
        _ ->
          model
    FahrenheitChange newFahrenheit ->
      case String.toFloat newFahrenheit of
        Just f ->
          { model | celsium = toCelsius f, fahrenheit = f }
        _ ->
          model
      

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ 
        div [] [ 
            div [] [text ("Temparature converter")]
          , label [] [text ("Celsium")]
          , input [ type_ "number", value (String.fromFloat model.celsium), onInput CelsiumChange ] []
          , label [] [text ("Fahrenheit")]
          , input [ type_ "number", value (String.fromFloat model.fahrenheit), onInput FahrenheitChange  ] []
         ]
    ]
{-
(optional) Eight Queens

Very famous Eight Queens Problem. Please see https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/p/p90.html for details.
-}

queens :  List (List Int)
queens = 
  let
    -- TODO
    list = []
  in
    list
