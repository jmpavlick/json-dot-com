module Dropdown exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id, name, selected)
import Json.DotCom
import Json.Encode
import Set exposing (Set)



-- APP ALIAS


type alias App x =
    { x | xorSet : Set String }



-- VIEW


view : { name : String } -> App x -> List ( String, Html msg ) -> Html msg
view args ({ xorSet } as app) options =
    let
        toXorKey : String -> String
        toXorKey label =
            args.name ++ ":" ++ label

        isShowingKey : String
        isShowingKey =
            args.name ++ ":isShowingAllOptions"

        isShowing : Bool
        isShowing =
            Set.member isShowingKey xorSet

        selectedOption : Maybe ( String, Html msg )
        selectedOption =
            List.filter (\( label, _ ) -> Set.member (toXorKey label) xorSet) options
                |> List.head

        selectedLabel : String
        selectedLabel =
            case selectedOption of
                Just ( label, _ ) ->
                    label

                Nothing ->
                    "Select an option..."
    in
    div [ class "relative" ]
        [ -- Dropdown button
          a
            [ Json.DotCom.href isShowingKey
            , class "block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm bg-white text-gray-900 text-left cursor-pointer hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
            ]
            [ div [ class "flex justify-between items-center" ]
                [ text selectedLabel
                , span [ class "text-gray-400" ]
                    [ text
                        (if isShowing then
                            "▲"

                         else
                            "▼"
                        )
                    ]
                ]
            ]
        , -- Dropdown options (only show when isShowing is true)
          if isShowing then
            div [ class "absolute z-50 mt-1 w-full bg-white border border-gray-300 rounded-md shadow-lg max-h-60 overflow-y-auto" ]
                (List.map
                    (\( label, optionView ) ->
                        let
                            xorKey : String
                            xorKey =
                                toXorKey label

                            isSelected : Bool
                            isSelected =
                                Set.member xorKey xorSet
                        in
                        a
                            [ Json.DotCom.href xorKey
                            , class
                                ("block px-3 py-2 hover:bg-gray-100 cursor-pointer "
                                    ++ (if isSelected then
                                            "bg-blue-50 text-blue-700"

                                        else
                                            "text-gray-900"
                                       )
                                )
                            ]
                            [ optionView ]
                    )
                    options
                )

          else
            text ""
        ]



-- LIFECYCLE


onUrlRequest : { name : String } -> App x -> Browser.UrlRequest -> Result Browser.UrlRequest ( App x, Cmd msg )
onUrlRequest { name } ({ xorSet } as app) =
    let
        isShowingKey : String
        isShowingKey =
            name ++ ":isShowingAllOptions"
    in
    Json.DotCom.onUrlRequest
        (\str ->
            if str == isShowingKey then
                -- Toggle dropdown open/closed
                Just
                    ( { app
                        | xorSet =
                            if Set.member str xorSet then
                                Set.remove str xorSet

                            else
                                Set.insert str xorSet
                      }
                    , Cmd.none
                    )

            else if String.startsWith (name ++ ":") str && str /= isShowingKey then
                -- Select an option and close dropdown
                let
                    -- Remove all other option selections for this dropdown
                    clearedXorSet =
                        Set.filter (\key -> not (String.startsWith (name ++ ":") key) || key == isShowingKey) xorSet

                    -- Remove the showing state (close dropdown) and add the selected option
                    newXorSet =
                        clearedXorSet
                            |> Set.remove isShowingKey
                            |> Set.insert str
                in
                Just
                    ( { app | xorSet = newXorSet }
                    , Cmd.none
                    )

            else
                Nothing
        )
