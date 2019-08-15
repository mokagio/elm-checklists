import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = initialState, update = update, view = view }

initialState =
  [ "first"
  , "second"
  , "last"
  ]

update msg model =
  model

view model =
  -- TODO: is there something like <- that I can use? instead of wrapping in ()
  div [] (List.map (\n -> div [] [ text n ]) model)
