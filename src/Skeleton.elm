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
            , afterHeader
            ]
        ]
        [ viewLogo ]


afterHeader : Style
afterHeader =
    after
        [ position absolute
        , bottom (px 0)
        , left (pct 50)
        , width (px 60)
        , height (px 1)
        , backgroundColor (hex "000")
        , transform (translateX (pct -50))
        , property "content" "''"
        ]


viewLogo : Html msg
viewLogo =
    div [ css [ margin auto ] ]
        [ h1
            [ css
                [ fontFamilies [ "DejaVu Sans" ]
                , fontWeight normal
                , fontSize (rem 2)
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
