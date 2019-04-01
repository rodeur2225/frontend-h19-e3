module Api.Endpoint exposing (Endpoint, bookListing, createListing, listing, listings, request)

import Http
import Url.Builder exposing (QueryParameter)


request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , url : Endpoint
    }
    -> Cmd a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , url = unwrap config.url
        , timeout = Nothing
        , tracker = Nothing
        }



-- TYPES


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin "https://glo2003h19e3.herokuapp.com/" paths queryParams |> Endpoint



-- ENDPOINTS


listings : Endpoint
listings =
    url [ "listings" ] []


listing : String -> Endpoint
listing id =
    url [ "listings", id ] []


bookListing : String -> Endpoint
bookListing id =
    url [ "listings", id, "book" ] []


createListing : Endpoint
createListing =
    url [ "listings" ] []
