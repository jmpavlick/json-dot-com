module Json.DotCom exposing
    ( encodeAsHref, href
    , onUrlRequest, onEncodedUrlRequest
    , batch
    )

{-|


# IT'S JSON DOT COM BABY


# Make hrefs

@docs encodeAsHref, href


# Build handlers

@docs onUrlRequest, onEncodedUrlRequest


# Apply handlers

@docs batch

-}

import Browser
import Html
import Html.Attributes
import Json.Decode
import Json.Encode
import List.Extra
import Parser exposing ((|.), (|=))
import Url


{-| -}
href : String -> Html.Attribute msg
href str =
    Html.Attributes.href <| toToken str


{-| -}
encodeAsHref : (a -> Json.Encode.Value) -> a -> Html.Attribute msg
encodeAsHref encoder value =
    href <| Json.Encode.encode 0 (encoder value)


{-| -}
onEncodedUrlRequest : Json.Decode.Decoder a -> Browser.UrlRequest -> Result Browser.UrlRequest a
onEncodedUrlRequest decoder request =
    case request of
        Browser.Internal _ ->
            Err request

        Browser.External someExternalUrl ->
            Result.mapError (always request) <|
                parseDecodeHref decoder someExternalUrl


{-| -}
toOnEncodedUrlRequest : { path : String } -> Json.Decode.Decoder a -> Browser.UrlRequest -> Result Browser.UrlRequest a
toOnEncodedUrlRequest path decoder request =
    case request of
        Browser.Internal _ ->
            Err request

        Browser.External someExternalUrl ->
            Result.mapError (always request) <|
                toParseDecodeHref path decoder someExternalUrl


{-| this is a doc comment
-}
parseHref : String -> Maybe String
parseHref =
    toParseHref { path = "" }


{-| this is a doc comment
-}
parseDecodeHref : Json.Decode.Decoder a -> String -> Result Json.Decode.Error a
parseDecodeHref =
    toParseDecodeHref { path = "" }


{-| -}
onUrlRequest : Browser.UrlRequest -> Result Browser.UrlRequest String
onUrlRequest request =
    case request of
        Browser.Internal _ ->
            Err request

        Browser.External someExternalUrl ->
            case parseHref someExternalUrl of
                Nothing ->
                    Err request

                Just jsonStr ->
                    Ok jsonStr


{-| -}
toUrlRequest : { path : String } -> Browser.UrlRequest -> Result Browser.UrlRequest String
toUrlRequest path request =
    case request of
        Browser.Internal _ ->
            Err request

        Browser.External someExternalUrl ->
            case toParseHref path someExternalUrl of
                Nothing ->
                    Err request

                Just jsonStr ->
                    Ok jsonStr


toParseHref : { path : String } -> String -> Maybe String
toParseHref { path } hrefStr =
    Result.toMaybe <|
        Parser.run
            (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.token (toToken path)
                    |. Parser.chompWhile (always True)
            )
            hrefStr


toParseDecodeHref : { path : String } -> Json.Decode.Decoder a -> String -> Result Json.Decode.Error a
toParseDecodeHref path decoder hrefStr =
    case toParseHref path hrefStr of
        Nothing ->
            Err <|
                Json.Decode.Failure
                    "The input string was not a valid json-dot-com-encoded value. I couldn't find 'https://json.com/' anywhere!"
                    (Json.Encode.string hrefStr)

        Just jsonStr ->
            Json.Decode.decodeString decoder jsonStr


{-| -}
toHref : { path : String } -> Json.Encode.Value -> Html.Attribute msg
toHref { path } value =
    Html.Attributes.href <| toToken path ++ Json.Encode.encode 0 value


{-| -}
toEncodeAsHref : { path : String } -> (a -> Json.Encode.Value) -> a -> Html.Attribute msg
toEncodeAsHref path encoder value =
    toHref path <| encoder value



-- INTERNALS


toToken : String -> String
toToken path =
    (case path of
        "" ->
            identity

        str ->
            \j -> j ++ str ++ "/"
    )
        "https://json.com/"



--


{-| -}
batch :
    { onBrowserInternal : Url.Url -> a, onBrowserExternal : String -> a }
    -> List (Browser.UrlRequest -> Result Browser.UrlRequest a)
    -> Browser.UrlRequest
    -> a
batch { onBrowserInternal, onBrowserExternal } matchers bUrlRequest =
    let
        matchResult =
            List.Extra.stoppableFoldl
                (\stepMatcher acc ->
                    case stepMatcher bUrlRequest of
                        Ok parsed ->
                            List.Extra.Stop (Ok parsed)

                        Err _ ->
                            List.Extra.Continue acc
                )
                (Err bUrlRequest)
                matchers
    in
    case matchResult of
        Err bur ->
            case bur of
                Browser.Internal url ->
                    onBrowserInternal url

                Browser.External str ->
                    onBrowserExternal str

        Ok parsed ->
            parsed
