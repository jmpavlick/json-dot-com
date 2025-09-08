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


stopPropogationKey : String
stopPropogationKey =
    "clicked-non-interactive-content-on-big-ol-modal"



-- VIEW


view : String -> List (Html msg) -> Html msg
view title body =
    node "dialog"
        [ id showKey
        , class "p-0 bg-transparent border-0 max-w-none w-full h-full backdrop:bg-transparent"
        ]
        [ -- Backdrop (clickable to close, fills the dialog)
          a
            [ Json.DotCom.href hideKey
            , class "fixed inset-0 bg-black bg-opacity-50"
            ]
            []
        , -- Modal content (positioned over backdrop)
          div
            [ class "fixed inset-0 flex items-center justify-center pointer-events-none" ]
            [ div
                [ class "bg-white rounded-lg shadow-xl mx-4 max-w-md w-full max-h-screen overflow-hidden pointer-events-auto" ]
                [ -- Header
                  div
                    [ class "px-6 py-4 border-b border-gray-200" ]
                    [ h3 [ class "text-lg font-semibold text-gray-900" ] [ text title ] ]
                , -- Body
                  div
                    [ class "px-6 py-4 overflow-y-auto" ]
                    body
                ]
            ]
        ]


launcher : String -> List (Html msg) -> Html msg
launcher title body =
    div []
        [ a [ Json.DotCom.href showKey ] [ text "Edit" ]
        , view title body
        ]



-- LIFECYCLE


type alias App x =
    { x | xorSet : Set String }


onUrlRequest : App x -> Browser.UrlRequest -> Result Browser.UrlRequest ( App x, Cmd msg )
onUrlRequest app =
    Json.DotCom.onUrlRequest
        (\str ->
            if str == showKey then
                Just ( app, openDialog showKey )

            else if str == hideKey then
                Just ( app, closeDialog showKey )

            else if str == stopPropogationKey then
                Just ( app, Cmd.none )

            else
                Nothing
        )



-- PORTS
-- NOTE: the ports are NOT required for `Json.DotCom` to work - they're an implementation detail of the modal


port openDialog : String -> Cmd msg


port closeDialog : String -> Cmd msg
