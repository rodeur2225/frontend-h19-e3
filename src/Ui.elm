module Ui exposing (input, label, submit, textarea)

import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css, type_)


label : List (Attribute msg) -> List (Html msg) -> Html msg
label attributes children =
    let
        styles =
            css
                [ display block
                , textTransform uppercase
                , fontFamilies [ "DejaVu Sans" ]
                , fontWeight bold
                , fontSize (rem 0.8)
                , marginTop (px 12)
                ]
    in
    Html.label (styles :: attributes) children


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attributes children =
    let
        styles =
            css inputCss
    in
    Html.input (styles :: attributes) children


submit : List (Attribute msg) -> List (Html msg) -> Html msg
submit attributes children =
    let
        styles =
            css
                [ backgroundColor transparent
                , margin (px 8)
                , padding2 (px 8) (px 12)
                , borderRadius (px 20)
                , border (px 0)
                , cursor pointer
                , fontWeight bold
                , fontSize (rem 1)
                , textTransform uppercase
                , hover
                    [ boxShadow4 (px 0) (px 1) (px 3) (rgba 0 0 0 0.5) ]
                ]
    in
    Html.input (type_ "submit" :: styles :: attributes) children


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea attributes children =
    let
        styles =
            css inputCss
    in
    Html.textarea (styles :: attributes) children


inputCss : List Style
inputCss =
    [ display block
    , width (pct 100)
    , border (px 0)
    , marginBottom (px 18)
    , padding (px 8)
    , fontFamilies [ "DejaVu Sans" ]
    ]
