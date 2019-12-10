module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, field, string, int, map2)

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

{--
type alias Model =
  { person: Maybe Person
  , msg: String
  }
--}

type alias ListModel =
  { people: List Person
  , msg: String
  }

type Msg
  = FetchPeople
  | GotPeople (Result Http.Error (List Person))
  | GotPerson (Result Http.Error Person)


startIt: () -> (ListModel, Cmd Msg)
startIt _ = (ListModel [] "OK", Cmd.none)
-- startIt _ = ({ people = [], msg = "OK" }, Cmd.none)

updateIt:  Msg -> ListModel -> (ListModel, Cmd Msg)
updateIt message model =
  case message of
    FetchPeople -> (model, fetchPerson)

    GotPeople (Ok p) -> ({ model | people = p }, Cmd.none)

    GotPeople (Err err) -> ({ model | msg = (printError err) }, Cmd.none)

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

fetchPeople: Cmd Msg
fecthPeople =
  Http.get
    { url = "people.json"
    , expect = Htpp.expectJson GotPeople personListDecoder
    }

personDecoder: Decoder Person
personDecoder =
  map2 Person
    (field "name" string)
    (field "age" int)

personListDecoder: Decoder (List Person)
personListDecoder =
  JD.list personDecoder

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
