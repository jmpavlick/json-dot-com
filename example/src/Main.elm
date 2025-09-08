module Main exposing (..)

import Browser
import Browser.Navigation
import ColorPicker
import Endo exposing (Endo)
import Html exposing (..)
import Html.Attributes exposing (href)
import Json.DotCom
import Modal
import Set exposing (Set)
import Url


c : String -> Html.Attribute msg
c =
    Html.Attributes.class


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , xorSet = Set.empty
      , colorPicker = ColorPicker.init
      , url = url
      }
    , Cmd.none
    )


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url


type alias Model =
    { key : Browser.Navigation.Key
    , xorSet : Set String
    , colorPicker : ColorPicker.Model
    , url : Url.Url
    }


toggle : String -> Endo (Set String)
toggle key set =
    if Set.member key set then
        Set.remove key set

    else
        Set.insert key set


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest urlRequest ->
            Json.DotCom.handle
                { onBrowserInternal = \url -> ( { model | url = url }, Browser.Navigation.pushUrl model.key (Url.toString url) )
                , onBrowserExternal = \str -> ( model, Browser.Navigation.load str )
                }
                (Json.DotCom.batch
                    [ Modal.onUrlRequest model
                    , ColorPicker.onUrlRequest model
                    ]
                )
                urlRequest

        OnUrlChange url ->
            ( { model | url = url }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "hello"
    , body =
        [ div [] [ text "elm is a pl that i like" ]
        , a [ href "#warble" ] [ text "click the warble" ]
        , div [] [ text <| Debug.toString model.url ]
        , div [] [ text <| Debug.toString model.xorSet ]
        , Modal.launcher { label = "Show modal" }
            "This is a modal"
            [ div [] [ text "This is some content", ColorPicker.view model ] ]
        ]
    }
