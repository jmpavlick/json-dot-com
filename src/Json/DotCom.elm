module Json.DotCom exposing
    ( encodeAsHref, href
    , onEncodedUrlRequest, handleEncodedUrlRequest
    , urlRequest, handleUrlRequest
    , toEncodeAsHref, toHref
    , toHandleEncodedUrlRequest, toOnEncodedUrlRequest
    , toUrlRequest, toHandleUrlRequest
    )

{-|


# IT'S JSON DOT COM BABY


# Make hrefs

@docs encodeAsHref, href


# Handle hrefs

@docs onEncodedUrlRequest, handleEncodedUrlRequest


# Handle hrefs (string payload)

@docs urlRequest, handleUrlRequest


# Roll your own hrefs

@docs toEncodeAsHref, toHref


# Roll your own handlers

@docs toHandleEncodedUrlRequest, toOnEncodedUrlRequest


# Roll your own handlers (string payload)

@docs toUrlRequest, toHandleUrlRequest

-}

import Browser
import Html
import Html.Attributes
import Json.Decode
import Json.Encode
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
handleEncodedUrlRequest :
    { onBrowserInternal : Url.Url -> b
    , onBrowserExternal : String -> b
    , onDecodeSucceeded : a -> b
    , onDecodeFailed : Json.Decode.Error -> b
    }
    -> Json.Decode.Decoder a
    -> Browser.UrlRequest
    -> b
handleEncodedUrlRequest { onBrowserInternal, onBrowserExternal, onDecodeSucceeded, onDecodeFailed } decoder request =
    case request of
        Browser.Internal url ->
            onBrowserInternal url

        Browser.External someExternalUrl ->
            case parseHref someExternalUrl of
                Nothing ->
                    onBrowserExternal someExternalUrl

                Just jsonStr ->
                    case Json.Decode.decodeString decoder jsonStr of
                        Err decodeErr ->
                            onDecodeFailed decodeErr

                        Ok hellYeah ->
                            onDecodeSucceeded hellYeah


{-| -}
urlRequest : Browser.UrlRequest -> Result Browser.UrlRequest String
urlRequest request =
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


{-| -}
handleUrlRequest :
    { onBrowserInternal : Url.Url -> b
    , onBrowserExternal : String -> b
    , onStringSucceeded : String -> b
    , onStringFailed : Json.Decode.Error -> b
    }
    -> Browser.UrlRequest
    -> b
handleUrlRequest { onBrowserInternal, onBrowserExternal, onStringSucceeded, onStringFailed } request =
    case request of
        Browser.Internal url ->
            onBrowserInternal url

        Browser.External someExternalUrl ->
            case parseHref someExternalUrl of
                Nothing ->
                    onBrowserExternal someExternalUrl

                Just jsonStr ->
                    onStringSucceeded jsonStr


{-| -}
toHandleUrlRequest :
    { path : String }
    ->
        { onBrowserInternal : Url.Url -> b
        , onBrowserExternal : String -> b
        , onStringSucceeded : String -> b
        , onStringFailed : Json.Decode.Error -> b
        }
    -> Browser.UrlRequest
    -> b
toHandleUrlRequest path { onBrowserInternal, onBrowserExternal, onStringSucceeded, onStringFailed } request =
    case request of
        Browser.Internal url ->
            onBrowserInternal url

        Browser.External someExternalUrl ->
            case toParseHref path someExternalUrl of
                Nothing ->
                    onBrowserExternal someExternalUrl

                Just jsonStr ->
                    onStringSucceeded jsonStr


{-| -}
toHandleEncodedUrlRequest :
    { path : String }
    ->
        { onBrowserInternal : Url.Url -> b
        , onBrowserExternal : String -> b
        , onDecodeSucceeded : a -> b
        , onDecodeFailed : Json.Decode.Error -> b
        }
    -> Json.Decode.Decoder a
    -> Browser.UrlRequest
    -> b
toHandleEncodedUrlRequest path { onBrowserInternal, onBrowserExternal, onDecodeSucceeded, onDecodeFailed } decoder request =
    case request of
        Browser.Internal url ->
            onBrowserInternal url

        Browser.External someExternalUrl ->
            case toParseHref path someExternalUrl of
                Nothing ->
                    onBrowserExternal someExternalUrl

                Just jsonStr ->
                    case Json.Decode.decodeString decoder jsonStr of
                        Err decodeErr ->
                            onDecodeFailed decodeErr

                        Ok hellYeah ->
                            onDecodeSucceeded hellYeah


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
