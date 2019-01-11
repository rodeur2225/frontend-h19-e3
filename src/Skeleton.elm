module Skeleton exposing (Details, view)

import Browser
import Html.Styled as Html exposing (Html, main_, toUnstyled)



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
        , content = [ Html.map toMsg <| main_ [] details.content ]
        }


unstyle : Details msg -> Browser.Document msg
unstyle doc =
    { title = doc.title, body = List.map toUnstyled doc.content }
