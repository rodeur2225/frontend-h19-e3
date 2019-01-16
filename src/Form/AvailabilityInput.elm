module Form.AvailabilityInput exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (checked, css, for, hidden, id, type_, value)
import Html.Styled.Events exposing (onCheck, onSubmit)
import Time



-- MODEL


type alias Model =
    Time.Posix



-- VIEW


view : List Model -> (Model -> msg) -> (Model -> msg) -> Model -> Html msg
view models addBooking removeBooking availability =
    let
        inputId =
            String.fromInt (Time.posixToMillis availability)

        day =
            String.fromInt (Time.toDay Time.utc availability)

        humanDay =
            toFrenchWeekday (Time.toWeekday Time.utc availability)

        booked =
            List.member availability models
    in
    div
        []
        [ input
            [ id inputId
            , type_ "checkbox"
            , hidden True
            , onCheck
                (\checked ->
                    if checked then
                        addBooking availability

                    else
                        removeBooking availability
                )
            ]
            []
        , label
            [ for inputId
            , css
                [ displayFlex
                , flexDirection column
                , alignItems center
                , borderRadius (px 20)
                , border3 (px 1) solid (rgba 0 0 0 0.05)
                , margin (px 8)
                , padding2 (px 4) (px 8)
                , fontSize (rem 1)
                , textDecoration none
                , color (hex "000")
                , bookedStyle booked
                , cursor pointer
                , hover
                    [ boxShadow4 (px 0) (px 1) (px 3) (rgba 0 0 0 0.5) ]
                ]
            ]
            [ span
                [ css
                    [ padding2 (px 8) (px 12)
                    , borderBottom3 (px 1) solid (rgba 0 0 0 0.05)
                    , fontSize (rem 1.2)
                    ]
                ]
                [ text day ]
            , span
                [ css
                    [ padding2 (px 8) (px 12)
                    , fontSize (rem 1)
                    ]
                ]
                [ text humanDay ]
            ]
        ]


bookedStyle : Bool -> Style
bookedStyle booked =
    if booked then
        backgroundColor (rgba 0 0 0 0.1)

    else
        backgroundColor transparent


toFrenchWeekday : Time.Weekday -> String
toFrenchWeekday weekday =
    case weekday of
        Time.Mon ->
            "Lun"

        Time.Tue ->
            "Mar"

        Time.Wed ->
            "Mer"

        Time.Thu ->
            "Jeu"

        Time.Fri ->
            "Ven"

        Time.Sat ->
            "Sam"

        Time.Sun ->
            "Dim"
