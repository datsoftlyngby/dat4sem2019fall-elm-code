module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

import Page.Editor as Editor
import Page.Listing as Listing

main = Browser.element
  { init = init
  , update = update
  , subscriptions = noSubscriptions
  , view = view
  }

type Model
  = Editor Editor.Model
  | Listing Listing.Model

type Msg
  = EditorMsg Editor.Msg
  | ListingMsg Listing.Msg


init : () -> (Model, Cmd Msg)
init _ = (Editor Editor.init, Cmd.none)

view : Model -> Html Msg
view model =
  case model of
    Editor mdl -> viewWith EditorMsg (Editor.view mdl)
    Listing mdl -> Listing.view mdl |> viewWith ListingMsg

viewWith : (msg -> Msg) -> (Html msg) -> (Html Msg)
viewWith messenger (msg) = Html.map messenger msg

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case (message, model) of
    (EditorMsg Editor.FetchPeople, _) ->
      Listing.update Listing.FetchPeople Listing.init
        |> updateWith Listing ListingMsg

    (ListingMsg Listing.FetchPerson, _) ->
      Editor.update Editor.FetchPerson Editor.init
        |> updateWith Editor EditorMsg

    (EditorMsg msg, Editor mdl) ->
      Editor.update msg mdl |> updateWith Editor EditorMsg

    (ListingMsg msg, Listing mdl) ->
      Listing.update msg mdl |> updateWith Listing ListingMsg

    (_, _) -> (model, Cmd.none)

updateWith : (mdl -> Model) -> (msg -> Msg) -> (mdl, Cmd msg) -> (Model, Cmd Msg)
updateWith modeller messenger (mdl, cmd) =
  (modeller mdl, Cmd.map messenger cmd)




{--
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
--}


noSubscriptions: Model -> Sub Msg
noSubscriptions _ = Sub.none
