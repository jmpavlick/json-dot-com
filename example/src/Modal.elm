port module Modal exposing (..)

-- don't panic

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Json.DotCom
import Set exposing (Set)


showKey : String
showKey =
    "show-big-ol-modal"


hideKey : String
hideKey =
    "close-big-ol-modal"



-- VIEW


view : String -> List (Html msg) -> Html msg
view title body =
    node "dialog"
        [ id showKey
        , class "p-0 bg-transparent border-0 max-w-none w-full h-full backdrop:bg-transparent"
        ]
        [ -- backdrop (clickable to close, fills the dialog)
          a
            [ Json.DotCom.href hideKey
            , class "fixed inset-0 bg-black bg-opacity-50"
            ]
            []
        , -- modal content (positioned over backdrop); if this isn't a sibling of the backdrop,
          -- the href will bubble up and that will be a bad time for everyone
          div
            [ class "fixed inset-0 flex items-center justify-center pointer-events-none" ]
            [ div
                [ class "bg-white rounded-lg shadow-xl mx-4 max-w-md w-full max-h-screen overflow-hidden pointer-events-auto" ]
                [ -- header
                  div
                    [ class "px-6 py-4 border-b border-gray-200" ]
                    [ h3 [ class "text-lg font-semibold text-gray-900" ] [ text title ] ]
                , -- body
                  div
                    [ class "px-6 py-4 overflow-y-auto" ]
                    body
                ]
            ]
        ]


launcher : { label : String } -> String -> List (Html msg) -> Html msg
launcher { label } title body =
    div []
        [ a [ Json.DotCom.href showKey ] [ text label ]
        , view title body
        ]



-- LIFECYCLE


onUrlRequest : model -> Browser.UrlRequest -> Result Browser.UrlRequest ( model, Cmd msg )
onUrlRequest model =
    Json.DotCom.onUrlRequest
        (\str ->
            if str == showKey then
                Just ( model, openDialog showKey )

            else if str == hideKey then
                Just ( model, closeDialog showKey )

            else
                Nothing
        )



-- PORTS
-- NOTE: the ports are NOT required for `Json.DotCom` to work - they're an implementation detail of the modal


port openDialog : String -> Cmd msg


port closeDialog : String -> Cmd msg
