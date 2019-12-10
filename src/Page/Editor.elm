module Page.Editor exposing (..)

import Person exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http

type alias Model =
  { person: Person
  , message: String
  }

type Msg
  = FetchPerson
  | GotPerson (Result Http.Error Person)
  | FetchPeople

init : Model
init = Model { id = 8, name = "Zeus", age = 2786 } "OK"

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "View or Edit a person"]
    , text (toString model.person)
    , br [] []
    , text model.message
    , hr [] []
    , button [onClick FetchPeople] [text "Back to list"]
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    FetchPerson -> (model, fetchPerson)
    GotPerson (Ok person) -> (model, Cmd.none)
    GotPerson (Err _) -> ({ model | message = "Not OK" }, Cmd.none)
    _ -> (model, Cmd.none)

fetchPerson: Cmd Msg
fetchPerson =
  Http.get
    { url = "person.json"
    , expect = Http.expectJson GotPerson personDecoder
    }
