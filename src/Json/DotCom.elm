module Json.DotCom exposing
    ( Handler
    , encodeAsHref, href
    , onUrlRequest, onEncodedUrlRequest
    , batch, handle
    )

{-|


# IT'S JSON DOT COM BABY

@docs Handler


# Make hrefs

@docs encodeAsHref, href


# Build handlers

@docs onUrlRequest, onEncodedUrlRequest


# Apply handlers

@docs batch, handle

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
type alias Handler a =
    Browser.UrlRequest -> Result Browser.UrlRequest a


{-| -}
href : String -> Html.Attribute msg
href str =
    Html.Attributes.href <| token ++ str


{-| -}
encodeAsHref : (a -> Json.Encode.Value) -> a -> Html.Attribute msg
encodeAsHref encoder value =
    href <| Json.Encode.encode 0 (encoder value)


{-| -}
onEncodedUrlRequest : Json.Decode.Decoder a -> Handler a
onEncodedUrlRequest decoder request =
    case request of
        Browser.Internal _ ->
            Err request

        Browser.External someExternalUrl ->
            Result.mapError (always request) <|
                parseDecodeHref decoder (Debug.log "onEncodedUrlRequest: someExternalUrl" someExternalUrl)


{-| -}
onUrlRequest : (String -> Maybe a) -> Handler a
onUrlRequest matcher request =
    case request of
        Browser.Internal _ ->
            Err request

        Browser.External someExternalUrl ->
            let
                maybeMatch : Maybe a
                maybeMatch =
                    Maybe.andThen matcher <|
                        Debug.log "parsed" <|
                            parseHref someExternalUrl
            in
            case maybeMatch of
                Nothing ->
                    Err request

                Just a ->
                    Ok a


{-| -}
batch : List (Handler a) -> Handler a
batch matchers bUrlRequest =
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


{-| -}
handle : { onBrowserInternal : Url.Url -> a, onBrowserExternal : String -> a } -> Handler a -> Browser.UrlRequest -> a
handle { onBrowserInternal, onBrowserExternal } matcher bUrlRequest =
    case matcher bUrlRequest of
        Err bur ->
            case bur of
                Browser.Internal url ->
                    onBrowserInternal url

                Browser.External str ->
                    onBrowserExternal str

        Ok parsed ->
            parsed



-- INTERNALS


token : String
token =
    "https://json.com/"


parseHref : String -> Maybe String
parseHref hrefStr =
    Result.toMaybe <|
        Parser.run
            (Parser.succeed identity
                |. Parser.token token
                |= (Parser.getChompedString <|
                        Parser.succeed ()
                            |. Parser.chompWhile (always True)
                   )
            )
            (Maybe.withDefault hrefStr <| Url.percentDecode hrefStr)


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
