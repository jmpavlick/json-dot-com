port module Modal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Json.DotCom
import Set exposing (Set)


key : String
key =
    "big-ol-modal"


view : String -> List (Html msg) -> Html msg
view title body =
    node "dialog"
        [ id key
        , class "p-0 bg-transparent border-0 max-w-md w-full backdrop:bg-black backdrop:bg-opacity-50"
        ]
        [ div
            [ class "bg-white rounded-lg shadow-xl mx-4 max-h-screen overflow-hidden" ]
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


launcher : String -> List (Html msg) -> Html msg
launcher title body =
    div []
        [ a [] [ text "Edit" ]
        , view title body
        ]



-- PORTS


port openDialog : String -> Cmd msg


port closeDialog : String -> Cmd msg
