module Json.DotCom exposing
    ( encodeAsHref, href
    , onUrlRequest, handleUrlRequest
    , toEncodeAsHref, toHref
    , toHandleUrlRequest, toOnUrlRequest
    )

{-|


# IT'S JSON DOT COM BABY


# Make hrefs

@docs encodeAsHref, href


# Handle hrefs

@docs onUrlRequest, handleUrlRequest


# Roll your own hrefs

@docs toEncodeAsHref, toHref


# Roll your own handlers

@docs toHandleUrlRequest, toOnUrlRequest

-}

import Browser
import Html
import Html.Attributes
import Json.Decode
import Json.Encode
import Parser exposing ((|.), (|=))
import Url


{-| -}
href : Json.Encode.Value -> Html.Attribute msg
href =
    toHref { path = "" }


{-| -}
encodeAsHref : (a -> Json.Encode.Value) -> a -> Html.Attribute msg
encodeAsHref encoder value =
    href <| encoder value


{-| -}
onUrlRequest : Json.Decode.Decoder a -> Browser.UrlRequest -> Result Browser.UrlRequest a
onUrlRequest decoder urlRequest =
    case urlRequest of
        Browser.Internal _ ->
            Err urlRequest

        Browser.External someExternalUrl ->
            Result.mapError (always urlRequest) <|
                parseDecodeHref decoder someExternalUrl


{-| -}
toOnUrlRequest : { path : String } -> Json.Decode.Decoder a -> Browser.UrlRequest -> Result Browser.UrlRequest a
toOnUrlRequest path decoder urlRequest =
    case urlRequest of
        Browser.Internal _ ->
            Err urlRequest

        Browser.External someExternalUrl ->
            Result.mapError (always urlRequest) <|
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
handleUrlRequest :
    { onBrowserInternal : Url.Url -> b
    , onBrowserExternal : String -> b
    , onDecodeSucceeded : a -> b
    , onDecodeFailed : Json.Decode.Error -> b
    }
    -> Json.Decode.Decoder a
    -> Browser.UrlRequest
    -> b
handleUrlRequest { onBrowserInternal, onBrowserExternal, onDecodeSucceeded, onDecodeFailed } decoder urlRequest =
    case urlRequest of
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
toHandleUrlRequest :
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
toHandleUrlRequest path { onBrowserInternal, onBrowserExternal, onDecodeSucceeded, onDecodeFailed } decoder urlRequest =
    case urlRequest of
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
