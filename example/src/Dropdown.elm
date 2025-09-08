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

        optionsWithSelected : List (Html msg)
        optionsWithSelected =
            List.map
                (\( label, optionView ) ->
                    let
                        xorKey : String
                        xorKey =
                            toXorKey label
                    in
                    option
                        [ selected <| Set.member xorKey xorSet
                        ]
                        [ a [ Json.DotCom.href xorKey ] [ optionView ] ]
                )
                options
    in
    select [ name args.name ] optionsWithSelected



-- LIFECYCLE


onUrlRequest : { name : String } -> App x -> Browser.UrlRequest -> Result Browser.UrlRequest ( App x, Cmd msg )
onUrlRequest { name } ({ xorSet } as app) =
    Json.DotCom.onUrlRequest
        (\str ->
            if String.startsWith (name ++ ":") str then
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

            else
                Nothing
        )
