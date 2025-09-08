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
        Browser.Internal someInternalUrl ->
            --Err request
            Result.mapError (always request) <|
                parseDecodeHref decoder someInternalUrl

        Browser.External _ ->
            Err request


{-| -}
onUrlRequest : (String -> Maybe a) -> Handler a
onUrlRequest matcher request =
    case request of
        Browser.Internal someInternalUrl ->
            let
                maybeMatch : Maybe a
                maybeMatch =
                    Maybe.andThen matcher <|
                        parseHref someInternalUrl
            in
            case maybeMatch of
                Nothing ->
                    Err request

                Just a ->
                    Ok a

        Browser.External _ ->
            Err request


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
    "/λ/"


parseHref : Url.Url -> Maybe String
parseHref { path } =
    Maybe.andThen
        (Result.toMaybe
            << Parser.run
                (Parser.succeed identity
                    |. Parser.token token
                    |= (Parser.getChompedString <|
                            Parser.succeed ()
                                |. Parser.chompWhile (always True)
                       )
                )
        )
        (Url.percentDecode path)


parseDecodeHref : Json.Decode.Decoder a -> Url.Url -> Result Json.Decode.Error a
parseDecodeHref decoder url =
    case parseHref url of
        Nothing ->
            Err <|
                Json.Decode.Failure
                    ("The input string was not a valid json-dot-com-encoded value. I couldn't find '"
                        ++ token
                        ++ "' anywhere!"
                    )
                    (Json.Encode.string (Url.toString url))

        Just jsonStr ->
            Json.Decode.decodeString decoder jsonStr
