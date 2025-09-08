module ColorPicker exposing (..)

import Browser
import Dropdown
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.DotCom
import Set exposing (Set)


type alias App x =
    { x | xorSet : Set String }


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
onUrlRequest =
    Dropdown.onUrlRequest key
