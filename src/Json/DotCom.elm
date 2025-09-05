module Json.DotCom exposing
    ( href, onUrlRequest
    , handleUrlRequest
    , toHref, parseHref, parseDecodeHref
    )

{-|


# IT'S JSON DOT COM BABY

@docs href, onUrlRequest

@docs handleUrlRequest

@docs toHref, parseHref, parseDecodeHref

-}

import Browser
import Html
import Html.Attributes
import Json.Decode
import Json.Encode
import Parser exposing ((|.), (|=))
import Url


{-| this is a doc comment
-}
href : Json.Encode.Value -> Html.Attribute msg
href value =
    Html.Attributes.href <| jsonDotCom ++ Json.Encode.encode 0 value


{-| this is a doc comment
-}
onUrlRequest : Json.Decode.Decoder a -> Browser.UrlRequest -> Result Browser.UrlRequest a
onUrlRequest decoder urlRequest =
    case urlRequest of
        Browser.Internal _ ->
            Err urlRequest

        Browser.External someExternalUrl ->
            Result.mapError (always urlRequest) <|
                parseDecodeHref decoder someExternalUrl


{-| this is a doc comment
-}
toHref : (a -> Json.Encode.Value) -> a -> Html.Attribute msg
toHref encoder value =
    href <| encoder value


{-| this is a doc comment
-}
parseHref : String -> Maybe String
parseHref hrefStr =
    Result.toMaybe <|
        Parser.run
            (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.token jsonDotCom
                    |. Parser.chompWhile (always True)
            )
            hrefStr


{-| this is a doc comment
-}
parseDecodeHref : Json.Decode.Decoder a -> String -> Result Json.Decode.Error a
parseDecodeHref decoder hrefStr =
    case parseHref hrefStr of
        Nothing ->
            Err <|
                Json.Decode.Failure
                    "The input string was not a valid json-dot-com-encoded value. I couldn't find 'https://json.com/' anywhere!"
                    (Json.Encode.string hrefStr)

        Just jsonStr ->
            Json.Decode.decodeString decoder jsonStr


{-| this is a doc comment
-}
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



-- INTERNALS


jsonDotCom : String
jsonDotCom =
    "https://json.com/"
