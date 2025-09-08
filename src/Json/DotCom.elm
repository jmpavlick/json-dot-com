module Json.DotCom exposing
    ( encodeAsHref, href
    , Handler
    , onUrlRequest, onEncodedUrlRequest
    , handle, batch
    )

{-|


## Another Elm package that could've been a blog post

I was reading the `elm/browser` docs the other night (as one does), and I noticed something in the [description of the `UrlRequest` type][UrlRequest docs] that I'd never noticed before (emphasis mine):

> All links in an [`application`](#application) create a `UrlRequest`. So
> when you click `<a href="/home">Home</a>`, **it does not just navigate!** It
> notifies `onUrlRequest` that the user wants to change the `Url`.

In 99.99999% of Elm apps that I've seen, there's a section of the toplevel `update` loop that looks like this:

    update msg model =
        case msg of
            LinkClicked urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        ( model, Browser.Navigation.pushUrl model.key (Url.toString url))

                    Browser.External str ->
                        ( model, Browser.Navigation.load str )

            UrlChanged url ->
                ( { model | url = url }
                , ...
                )

            ...

... which matches up to:

    main =
        Browser.application
            { onUrlRequest : LinkClicked
            , onUrlChange : UrlChanged
            ...

I've always wondered - why bother with having two handlers? We clicked a link, we got a `Url.Url`, which I am going to immediately call `Url.toString` on anyway and then navigate somewhere. For some reason, I need two variants on my toplevel `Msg` type for this. Seems redundant. Whatever. Felt like a bunch of ceremony for Evan Reasons.

But the bit about

> **it does not just navigate!**

caused some neuron activation. I had a Realization.

---

When I worked at Vendr, we had a `Ui.elm` module that was like... an ad-hoc implementation of `mdgriffith/elm-ui` 1.5 combined with a type alias

    type alias Html msg =
        Html (GlobalMsg msg)

for a type `GlobalMsg msg` that was defined something like

    type GlobalMsg msg
        = PageMsg msg
        | ModalMsg Modal.Msg
        | DropdownMsg Dropdown.Msg
        ...

so that we could have "globally-available" events, which could then be baked in to modules such as `Ui.Modal`, whose `view` function returned a `Ui.Html msg`. However, the `GlobalMsg` was in a module `GlobalMsg.elm`, and was imported by a module `Update.elm`, and as I'm sure you can imagine, we sometimes needed to perform effects / run `Cmd msg`s with our global handlers, so widening the `GlobalMsg msg` type would cause cascading compiler errors in multiple modules.

And of course, I am dramatically over-simplifying the nature of the thing; there was More To It (there always is), and some of these `update` functions were thousands of lines long. Those are always uncomfortable to change, because without exhausting amounts of let-decl-to-`Debug.todo`-ing, you're _going to_ get the type of something halfway through a 8-line composition wrong and it's going to blow the whole thing red as the LSP howls and your hot-reloader's error overlay's scrollbar shinks to a size too small to click on the first try.

I was getting ready to take a deep breath and dive back into those murky waters (instead of shipping my own SaaS product, I am once again trying to re-invent the idea of a flexible, composable components library), when I read the words

> **it does not just navigate!**

and then I was enlightened.


## We already have a `GlobalMsg msg`, it's already part of the platform

---

From a certain point of view, using a capital-M `Msg` type as the shape of events that can be emitted by a TEA structure is just a way of using the type system to guarantee that every Thing that you can Do, has a well-formed name and type. But it's not the _only_ way.

[UrlRequest docs]: https://package.elm-lang.org/packages/elm/browser/latest/Browser#UrlRequest
[application]: https://package.elm-lang.org/packages/elm/browser/latest/Browser#application


# Make hrefs

@docs encodeAsHref, href


# Build handlers

@docs Handler
@docs onUrlRequest, onEncodedUrlRequest


# Apply handlers

@docs handle, batch

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
