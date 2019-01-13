module Skeleton exposing (Details, view)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, href)



-- MODEL


type alias Details msg =
    { title : String
    , content : List (Html msg)
    }



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    unstyle
        { title = details.title ++ " - Sharie"
        , content =
            [ div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , fontFamilies [ "DejaVu Sans" ]
                    ]
                ]
                [ viewHeader, viewContent toMsg details.content, viewFooter ]
            ]
        }


viewHeader : Html msg
viewHeader =
    header
        [ css
            [ position relative
            , displayFlex
            , alignItems center
            , margin (px -6)
            , marginBottom (px 0)
            , boxShadow4 (px 0) (px 0) (px 10) (rgba 0 0 0 0.5)
            ]
        ]
        [ viewLogo ]


viewLogo : Html msg
viewLogo =
    div [ css [ margin auto ] ]
        [ h1
            [ css
                [ fontFamilies [ "DejaVu Sans" ]
                , fontWeight (int 300)
                , fontSize (rem 2.5)
                ]
            ]
            [ a [ href "/", css [ textDecoration none, color (hex "000") ] ] [ text "Sharie" ]
            ]
        ]


viewContent : (a -> msg) -> List (Html a) -> Html msg
viewContent toMsg content =
    Html.map toMsg <| main_ [] content


viewFooter : Html msg
viewFooter =
    footer [] []


unstyle : Details msg -> Browser.Document msg
unstyle doc =
    { title = doc.title, body = List.map toUnstyled doc.content }
