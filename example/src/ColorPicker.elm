module ColorPicker exposing (..)

import Browser
import Color
import Dropdown
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode
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
        (List.map
            (\( label, value ) ->
                ( label
                , a [ Json.DotCom.encodeAsHref Color.encoder value, class "block w-full" ] [ text label ]
                )
            )
            Color.values
        )


onUrlRequest : App x -> Browser.UrlRequest -> Result Browser.UrlRequest ( App x, Cmd msg )
onUrlRequest ({ xorSet, colorPicker } as app) =
    Json.DotCom.batch
        [ Result.map (Tuple.mapFirst (\a -> { app | xorSet = a.xorSet }))
            << Dropdown.onUrlRequest
                key
                { xorSet = xorSet }
        , Json.DotCom.onEncodedUrlRequest
            (Json.Decode.map
                (\color ->
                    ( { app | colorPicker = { selectedColor = Just color } }
                    , Cmd.none
                    )
                )
                Color.decoder
            )
        ]
