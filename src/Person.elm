module Person exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import String exposing (fromInt)

type alias Person =
  { id: Int
  , name: String
  , age: Int
  }

personDecoder : Decoder Person
personDecoder =
  JD.map3 Person
    (JD.field "id" JD.int)
    (JD.field "name" JD.string)
    (JD.field "age" JD.int)

personListDecoder : Decoder (List Person)
personListDecoder =
  JD.list personDecoder

personEncoder : Person -> Value
personEncoder person =
  JE.object
    [ ("id", JE.int person.id)
    , ("name", JE.string person.name)
    , ("age", JE.int person.age)
    ]

toString : Person -> String
toString person =
  "("++(fromInt person.id)++") "++person.name++" is of age "++(fromInt person.age)
