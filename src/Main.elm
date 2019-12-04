module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, field, string, int, map2)
import Json.Encode as JE

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

type alias EditModel =
  { person: Maybe Person
  , msg: String
  }

type alias ListModel =
  { people: List Person
  , msg: String
  }

type Model
  = EditModel
  | ListModel

type Msg
  = FetchPeople
--  | FetchPerson
  | GotPeople (Result Http.Error (List Person))
  | GotPerson (Result Http.Error Person)


startIt: () -> (Model, Cmd Msg)
startIt _ = (ListModel [] "OK", Cmd.none)
-- startIt _ = ({ people = [], msg = "OK" }, Cmd.none)

updateIt:  Msg -> Model -> (Model, Cmd Msg)
updateIt message model =
  case model of
    EditModel ->
      case message of
        FetchPeople -> (model, fetchPerson)

        GotPeople _ -> (model, Cmd.none)

        GotPerson _ -> (model, Cmd.none)
    ListModel ->
      case message of
        FetchPeople -> (model, fetchPeople)

        GotPeople (Ok p) -> ({ model | people = p }, Cmd.none)
        GotPeople (Err err) -> ({ model | msg = (printError err) }, Cmd.none)

        GotPerson _ -> (model, Cmd.none)

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
fetchPeople =
  Http.request
    { method = "GET"
    , headers = []
    , url = "people.json"
    , body = Http.emptyBody
    , expect = Http.expectJson GotPeople personListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

putPerson: Person -> Cmd Msg
putPerson person =
  Http.request
    { method = "PUT"
    , headers = []
    , url = "..."
    , body = Http.jsonBody (personEncoder person)
    , expect = Http.expectWhatever PersonPutted
    , timeout = Nothing
    , tracker = Nothing
    }

personDecoder: Decoder Person
personDecoder =
  map2 Person
    (field "name" string)
    (field "age" int)

personListDecoder: Decoder (List Person)
personListDecoder =
  JD.list personDecoder

personEncoder: Person -> JE.Value
personEncoder person =
  JE.object
    [ ("name", JE.string person.name)
    , ("age", JE.int person.age)
    ]

noSubscriptions: ListModel -> Sub Msg
noSubscriptions model =
  Sub.none

showIt: ListModel -> Html Msg
showIt model =
  div [] ( List.append
    [ text model.msg
    , hr [] []
    , button [onClick FetchPeople] [text "Click this"]
    , hr [] []
    ]
    (List.map showPerson model.people)
    )

showPerson: Person -> Html Msg
showPerson person =
  div []
    [ text person.name
    , text (String.fromInt person.age)
    ]

getName: Maybe Person -> String
getName mp =
  case mp of
    Just person -> person.name++" age: "++(String.fromInt person.age)
    Nothing ->     "Øhh"
