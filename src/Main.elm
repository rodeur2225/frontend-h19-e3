module Main exposing (Model)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html.Styled as Html
import Page.BookListing as BookListing
import Page.Listings as Listings
import Session
import Skeleton
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, string, top)



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound Session.Data
    | Listings Listings.Model
    | BookListing BookListing.Model



-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url { key = key, page = NotFound Session.empty }



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ListingsMsg Listings.Msg
    | BookListingMsg BookListing.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        ListingsMsg msg ->
            case model.page of
                Listings listings ->
                    stepListings model (Listings.update msg listings)

                _ ->
                    ( model, Cmd.none )

        BookListingMsg msg ->
            case model.page of
                BookListing listing ->
                    stepBookListing model (BookListing.update msg listing)

                _ ->
                    ( model, Cmd.none )


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        parser =
            oneOf
                [ route top (stepListings model (Listings.init session))
                , route (s "listings" </> string) (\id -> stepBookListing model (BookListing.init session id))
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound session }, Cmd.none )


stepListings : Model -> ( Listings.Model, Cmd Listings.Msg ) -> ( Model, Cmd Msg )
stepListings model ( listings, cmds ) =
    ( { model | page = Listings listings }
    , Cmd.map ListingsMsg cmds
    )


stepBookListing : Model -> ( BookListing.Model, Cmd BookListing.Msg ) -> ( Model, Cmd Msg )
stepBookListing model ( listing, cmds ) =
    ( { model | page = BookListing listing }
    , Cmd.map BookListingMsg cmds
    )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


exit : Model -> Session.Data
exit model =
    case model.page of
        NotFound session ->
            session

        Listings m ->
            m.session

        BookListing m ->
            m.session



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    case model.page of
        NotFound _ ->
            Skeleton.view never { title = "Not Found", content = [] }

        Listings listings ->
            Skeleton.view ListingsMsg (Listings.view listings)

        BookListing listing ->
            Skeleton.view BookListingMsg (BookListing.view listing)



-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
