module ColorPicker exposing (..)

import Browser
import Color
import Dropdown
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.DotCom
import Set exposing (Set)


type alias App x =
    { x
        | xorSet : Set String
        , colorPicker : Model
    }


type alias Model =
    { selectedColor : Maybe Color.Value }


init : Model
init =
    { selectedColor = Nothing }


key : { name : String }
key =
    { name = "color-picker" }


view : App x -> Html msg
view app =
    Dropdown.view key
        app
        [ ( "Red", div [] [ text "Red" ] )
        , ( "White", div [] [ text "White" ] )
        , ( "Blue", div [] [ text "Blue" ] )
        ]


onUrlRequest : App x -> Browser.UrlRequest -> Result Browser.UrlRequest ( App x, Cmd msg )
onUrlRequest ({ xorSet, colorPicker } as app) =
    Json.DotCom.batch
        [ \arg ->
            Result.map (Tuple.mapFirst (\a -> { app | xorSet = a.xorSet })) <|
                Dropdown.onUrlRequest
                    key
                    { xorSet = xorSet }
                    arg
        ]
