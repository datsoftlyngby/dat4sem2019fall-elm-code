module Page.Listing exposing (..)

import Person exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http

type alias Model =
  { people : List Person
  , message: String
  }

type Msg
  = FetchPeople
  | GotPeople (Result Http.Error (List Person))
  | FetchPerson

init : Model
init = Model [] "OK"

view : Model -> Html Msg
view model =
  div []
    (  [ h1 [] [text "List all people"] ]
    ++ (List.map viewPerson model.people)
    ++ [ text model.message
       , hr [] []
       , button [onClick FetchPerson] [text "Back to Person"]
       ]
    )

viewPerson : Person -> Html msg
viewPerson person =
  div [] [text (toString person)]

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    FetchPeople -> (model, fetchPeople)
    GotPeople (Ok people) -> ({ model | people = people }, Cmd.none)
    GotPeople (Err err) -> ({ model | message = (printError err) }, Cmd.none)
    _ -> (model, Cmd.none)

printError: Http.Error -> String
printError error =
  case error of
    Http.BadBody m -> "Bad body "++m
    Http.BadUrl u -> "Bad URL: "++u
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Network panic"
    Http.BadStatus i -> "Bad Status: "++(String.fromInt i)


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
