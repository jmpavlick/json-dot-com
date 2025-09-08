module Color exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Json.Decode
import Json.Encode


type alias Value =
    { background : String
    , text : String
    }


values : List ( String, Value )
values =
    [ ( "Modern"
      , { background = "bg-blue-600"
        , text = "text-white"
        }
      )
    , ( "Light"
      , { background = "bg-blue-100"
        , text = "text-blue-800"
        }
      )
    , ( "Dark"
      , { background = "bg-blue-800"
        , text = "text-blue-100"
        }
      )
    ]


withDefault : Maybe Value -> Value
withDefault =
    Maybe.withDefault { background = "", text = "" }


applyTheme : Value -> Browser.Document msg -> Browser.Document msg
applyTheme theme document =
    { title = document.title
    , body =
        [ Html.div [ class ("min-h-screen " ++ theme.background ++ " " ++ theme.text) ] document.body ]
    }


decoder : Json.Decode.Decoder Value
decoder =
    Json.Decode.map2 Value
        (Json.Decode.field "background" Json.Decode.string)
        (Json.Decode.field "text" Json.Decode.string)


encoder : Value -> Json.Encode.Value
encoder value =
    Json.Encode.object
        [ ( "background", Json.Encode.string value.background )
        , ( "text", Json.Encode.string value.text )
        ]
