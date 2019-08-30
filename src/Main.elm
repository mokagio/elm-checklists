module Main exposing (Checklist, ChecklistRun, Msg(..), Step, contentBody, defaultChecklist, initialState, isCompleted, main, makeViewModel, process, toListItem, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = initialState, update = update, view = view }


defaultChecklist =
    Checklist [ Step "first", Step "second", Step "third" ] "numbered"


initialState =
    { selectedChecklist = Nothing
    , checklists =
        [ defaultChecklist
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


view model =
    div [ class "container mx-auto py-8" ]
        [ h1 [ class "text-3xl pb-2" ] [ text "Checklists Demo" ]
        , p [ class "italic" ] [ text "Nothing of what you see is persisted ;)" ]
        , div [ class "py-4" ] [ contentBody model ]
        , div [ class "pt-4 pb-2" ] [ h3 [ class "text-xl" ] [ text "Next steps:" ] ]
        , ul [ class "list-disc list-inside" ] [ li [] [ text "reactive style" ], li [] [ text "create checklist" ], li [] [ text "re-run checklist and track run timestamps" ] ]
        ]


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


isCompleted checklistRun =
    checklistRun.currentStep >= List.length checklistRun.checklist.steps


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


process checklistRun =
    List.indexedMap
        (\index value -> makeViewModel value index checklistRun.currentStep)
        checklistRun.checklist.steps


makeViewModel step index currentIndex =
    if index < currentIndex then
        { text = step.name, completed = True, active = False }

    else if index == currentIndex then
        { text = step.name, completed = False, active = True }

    else
        { text = step.name, completed = False, active = False }
