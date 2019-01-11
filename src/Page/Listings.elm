module Page.Listings exposing (Model, Msg, init, update, view)

import Session
import Skeleton



-- MODEL


type alias Model =
    { session : Session.Data }



-- INIT


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( Model session, Cmd.none )



-- UPDATE


type Msg
    = String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details msg
view model =
    { title = "Listings", content = [] }
