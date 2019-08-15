import Browser
import Html exposing (Html, div, text, ul, li)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = initialState, update = update, view = view }

initialState =
  { steps =
    [ Step "first"
    , Step "second"
    , Step "last"
    ]
  , current = 1
  }

type alias Step =
  { name : String
  }

update msg model =
  model

view model =
  -- TODO: is there something like <- that I can use? instead of wrapping in ()
  ul [] (List.map toListItem (process model))

toListItem string =
  li [] [ text string ]

process state =
  List.indexedMap
    -- TODO: figure out the Elm Html way of doing strikthrough, if any, or add
    -- an attribute to the element
    (\index value -> if index >= state.current then value.name else "~~" ++ value.name ++ "~~")
    state.steps
