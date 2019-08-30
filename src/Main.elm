module Main exposing (Checklist, ChecklistRun, Msg(..), Step, contentBody, defaultChecklist, initialState, isCompleted, main, makeViewModel, process, toListItem, update, view)

{-| This is a playground experiment for me to learn
Elm and at the same time prototype a repeatable
checklist app.

Using these as reference:

  - <https://guide.elm-lang.org/architecture/>
  - <https://github.com/evancz/elm-todomvc/tree/07e3d4e5259f337d5eba781319b3a916e28aca99>

-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { selectedChecklist : Maybe ChecklistRun
    , checklists : List Checklist
    }


init : Model
init =
    { selectedChecklist = Nothing
    , checklists =
        [ Checklist [ Step "first", Step "second", Step "third" ] "numbered"
        , Checklist [ Step "some", Step "some more" ] "some and then some more"
        ]
    }


type alias ChecklistRun =
    { checklist : Checklist
    , currentStep : Int
    }


type alias Checklist =
    { steps : List Step
    , name : String
    }


type alias Step =
    { name : String
    }


type Msg
    = MoveToNext
    | Select Checklist


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveToNext ->
            case model.selectedChecklist of
                Nothing ->
                    model

                Just checklist ->
                    if checklist.currentStep < List.length checklist.checklist.steps then
                        { model | selectedChecklist = Just <| ChecklistRun checklist.checklist (checklist.currentStep + 1) }

                    else
                        model

        Select selectedChecklist ->
            { model | selectedChecklist = Just <| ChecklistRun selectedChecklist 0 }


view : Model -> Html Msg
view model =
    div [ class "container mx-auto py-8" ]
        [ h1 [ class "text-3xl pb-2" ] [ text "Checklists Demo" ]
        , p [ class "italic" ] [ text "Nothing of what you see is persisted ;)" ]
        , div [ class "py-4" ] [ contentBody model ]
        , div [ class "pt-4 pb-2" ] [ h3 [ class "text-xl" ] [ text "Next steps:" ] ]
        , ul [ class "list-disc list-inside" ] [ li [] [ text "reactive style" ], li [] [ text "create checklist" ], li [] [ text "re-run checklist and track run timestamps" ] ]
        ]


contentBody : Model -> Html Msg
contentBody model =
    case model.selectedChecklist of
        Nothing ->
            div []
                [ ul [] (List.map (\i -> li [] [ a [ href "#", onClick <| Select i ] [ text <| "👉 " ++ i.name ] ]) model.checklists)
                ]

        Just checklistRun ->
            div []
                [ div [] (List.map toListItem (process checklistRun))
                , div [ class "pt-2" ]
                    (if isCompleted checklistRun then
                        [ text "all done ✅" ]

                     else
                        []
                    )
                ]


isCompleted : ChecklistRun -> Bool
isCompleted checklistRun =
    checklistRun.currentStep >= List.length checklistRun.checklist.steps


toListItem : { text : String, completed : Bool, active : Bool } -> Html Msg
toListItem viewModel =
    div []
        -- TODO: using viewModel.text for the id might result in inconsistencies if
        -- there are multiple steps with the same name
        [ input
            [ type_ "checkbox"
            , class "mr-2"
            , checked viewModel.completed
            , disabled <| not viewModel.active
            , id viewModel.text
            ]
            []

        -- TODO: is it safe to always move to next? what if the user somehow
        -- manages to have the checkbox checked? should that be a MoveToPrevious?
        , label [ for viewModel.text, onClick MoveToNext ] [ text viewModel.text ]
        ]


process : ChecklistRun -> List { text : String, completed : Bool, active : Bool }
process checklistRun =
    List.indexedMap
        (\index value -> makeViewModel value index checklistRun.currentStep)
        checklistRun.checklist.steps


makeViewModel : Step -> Int -> Int -> { text : String, completed : Bool, active : Bool }
makeViewModel step index currentIndex =
    if index < currentIndex then
        { text = step.name, completed = True, active = False }

    else if index == currentIndex then
        { text = step.name, completed = False, active = True }

    else
        { text = step.name, completed = False, active = False }
