port module Modal exposing (launcher, onUrlRequest)

-- don't panic

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Json.DotCom
import Set exposing (Set)


openDialogKey : String
openDialogKey =
    "show-big-ol-modal"


closeDialogKey : String
closeDialogKey =
    "close-big-ol-modal"



-- VIEW


view : String -> List (Html msg) -> Html msg
view title body =
    node "dialog"
        [ id openDialogKey
        , class "p-0 bg-transparent border-0 max-w-none w-full h-full backdrop:bg-transparent"
        ]
        [ -- backdrop (clickable to close, fills the dialog)
          a
            [ Json.DotCom.href closeDialogKey
            , class "fixed inset-0 bg-black bg-opacity-50"
            ]
            []
        , -- modal content (positioned over backdrop); if this isn't a sibling of the backdrop,
          -- the href will bubble up and that will be a bad time for everyone
          div
            [ class "fixed inset-0 flex items-center justify-center pointer-events-none" ]
            [ div
                [ class "bg-white rounded-lg shadow-xl w-1/4 h-1/4 overflow-visible pointer-events-auto flex flex-col" ]
                [ -- header
                  div
                    [ class "px-6 py-4 border-b border-gray-200" ]
                    [ h3 [ class "text-lg font-semibold text-gray-900" ] [ text title ] ]
                , -- body
                  div
                    [ class "px-6 py-4 overflow-y-auto flex-1" ]
                    body
                ]
            ]
        ]


launcher : { label : String } -> String -> List (Html msg) -> Html msg
launcher { label } title body =
    div []
        [ a [ Json.DotCom.href openDialogKey ] [ text label ]
        , view title body
        ]



-- LIFECYCLE


onUrlRequest : model -> Json.DotCom.Handler ( model, Cmd msg )
onUrlRequest model =
    Json.DotCom.onUrlRequest
        (\str ->
            if str == openDialogKey then
                Just ( model, openDialog openDialogKey )

            else if str == closeDialogKey then
                Just ( model, closeDialog openDialogKey )

            else
                Nothing
        )



-- PORTS
-- NOTE: the ports are NOT required for `Json.DotCom` to work - they're an implementation detail of the modal


port openDialog : String -> Cmd msg


port closeDialog : String -> Cmd msg
