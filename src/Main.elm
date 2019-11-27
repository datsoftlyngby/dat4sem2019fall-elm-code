module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string, int, list, map2)

main = Browser.element
  { init = startIt
  , update = updateIt
  , subscriptions = noSubscriptions
  , view = showIt
  }

type alias Person =
  { name: String
  , age: Int
  }

type alias Model =
  { person: Maybe Person
  , msg: String
  }

type Msg
  = FetchPerson
  | GotPerson (Result Http.Error Person)


startIt: () -> (Model, Cmd Msg)
startIt _ = (Model Nothing "OK", Cmd.none)

updateIt:  Msg -> Model -> (Model, Cmd Msg)
updateIt message model =
  case message of
    FetchPerson -> (model, fetchPerson)

    GotPerson (Ok p) -> ({ model | person = Just p }, Cmd.none)

    GotPerson (Err err) -> ({ model | msg = (printError err) }, Cmd.none)

printError: Http.Error -> String
printError error =
  case error of
    Http.BadBody m -> "Bad body "++m
    Http.BadUrl u -> "Bad URL: "++u
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Network panic"
    Http.BadStatus i -> "Bad Status: "++(String.fromInt i)



fetchPerson: Cmd Msg
fetchPerson =
  Http.get
    { url = "person.json"
    , expect = Http.expectJson GotPerson personDecoder
    }

personDecoder: Decoder Person
personDecoder =
  map2 Person
    (field "name" string)
    (field "age" int)

noSubscriptions: Model -> Sub Msg
noSubscriptions model =
  Sub.none

showIt: Model -> Html Msg
showIt model =
  div []
    [ text ("#2 Person is "++(getName model.person))
    , br [] []
    , text model.msg
    , hr [] []
    , button [onClick FetchPerson] [text "Click this"]
    ]

getName: Maybe Person -> String
getName mp =
  case mp of
    Just person -> person.name++" age: "++(String.fromInt person.age)
    Nothing ->     "Øhh"
