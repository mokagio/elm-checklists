import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = t, update = update, view = view }

t =
  [ "first"
  , "second"
  , "last"
  ]

update msg model =
  model

view model =
  case List.head model of
    Nothing ->
      div []
      [ div [] [ text "empty" ]
      ]
    Just value ->
      div []
      [ div [] [ text value ]
      ]
