module Main exposing (..)

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
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Json.Decode as Json
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { mode : Mode
    , checklists : List Checklist
    , inProgressList : List ChecklistRun
    }


type Mode
    = Create NewChecklistParameters
    | Run ChecklistRun
    | Browse


init : Model
init =
    { mode = Browse
    , checklists =
        [ Checklist
            "Swimming Pool Packing"
            0
            [ Step "Towel", Step "Swim suit", Step "Goggles", Step "Body wash" ]
            Nothing
        , Checklist
            "Morning Routine"
            1
            [ Step "Meditate", Step "Brew coffee", Step "Journal", Step "Check emails" ]
            Nothing
        ]
    , inProgressList = []
    }


type alias ChecklistRun =
    { checklist : Checklist
    , currentStep : Int
    }


type alias Checklist =
    { name : String
    , uid : Int
    , steps : List Step
    , lastCompleted : Maybe Time.Posix
    }


type alias Step =
    { name : String
    }


type alias NewChecklistParameters =
    { name : String
    , editingName : Maybe String
    , steps : List Step
    , editingStep : Maybe Step
    }



-- TODO: It's hard to understand the group to which these Msgs belong to


type Msg
    = MoveToNext
    | ActuallyMoveToNext Time.Posix
    | Discard Checklist
    | BackHome
    | Select Checklist
    | CreateChecklist
    | UpdateChecklist CreateMsg
    | SaveChecklist
    | DiscardChecklist


type CreateMsg
    = UpdateTitle String
    | SaveTitle
    | UpdateStep String
    | SaveStep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveToNext ->
            ( model, Task.perform ActuallyMoveToNext Time.now )

        ActuallyMoveToNext time ->
            case model.mode of
                Run checklist ->
                    let
                        stepsLength =
                            List.length checklist.checklist.steps
                    in
                    if checklist.currentStep < stepsLength then
                        let
                            nextStep =
                                checklist.currentStep + 1

                            completed =
                                nextStep == stepsLength

                            updatedChecklistRun =
                                { checklist | currentStep = nextStep }
                        in
                        if completed then
                            let
                                updatedChecklists : Checklist -> Checklist
                                updatedChecklists c =
                                    if c.uid == checklist.checklist.uid then
                                        { c | lastCompleted = Just <| time }

                                    else
                                        c
                            in
                            ( { model
                                | mode = Run updatedChecklistRun
                                , checklists = List.map updatedChecklists model.checklists
                                , inProgressList = List.filter (\run -> run.checklist.uid /= updatedChecklistRun.checklist.uid) model.inProgressList
                              }
                            , Cmd.none
                            )

                        else
                            let
                                updatedInProgress : ChecklistRun -> ChecklistRun
                                updatedInProgress run =
                                    if run.checklist.uid == updatedChecklistRun.checklist.uid then
                                        updatedChecklistRun

                                    else
                                        run
                            in
                            ( { model
                                | mode = Run updatedChecklistRun
                                , inProgressList = List.map updatedInProgress model.inProgressList
                              }
                            , Cmd.none
                            )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Select selectedChecklist ->
            case List.head <| List.filter (\run -> run.checklist.uid == selectedChecklist.uid) model.inProgressList of
                Nothing ->
                    let
                        newRun =
                            ChecklistRun selectedChecklist 0
                    in
                    ( { model
                        | mode = Run newRun
                        , inProgressList = List.append model.inProgressList [ newRun ]
                      }
                    , Cmd.none
                    )

                Just inProgressRun ->
                    ( { model | mode = Run inProgressRun }
                    , Cmd.none
                    )

        Discard checklist ->
            ( { model
                | mode = Browse
                , inProgressList = List.filter (\run -> run.checklist.uid /= checklist.uid) model.inProgressList
              }
            , Cmd.none
            )

        BackHome ->
            case model.mode of
                Run checklistRun ->
                    ( { model | mode = Browse }
                    , Cmd.none
                    )

                _ ->
                    ( { model | mode = Browse }
                    , Cmd.none
                    )

        CreateChecklist ->
            ( { model | mode = Create <| NewChecklistParameters "" (Just "") [] Nothing }
            , Cmd.none
            )

        UpdateChecklist createMsg ->
            case model.mode of
                Create parameters ->
                    ( { model | mode = Create <| updateCreate createMsg parameters }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SaveChecklist ->
            case model.mode of
                Create parameters ->
                    ( { model
                        | mode = Browse
                        , checklists =
                            List.append
                                model.checklists
                                [ Checklist
                                    parameters.name
                                    (List.length model.checklists + 1)
                                    parameters.steps
                                    Nothing
                                ]
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DiscardChecklist ->
            ( { model | mode = Browse }, Cmd.none )


updateCreate : CreateMsg -> NewChecklistParameters -> NewChecklistParameters
updateCreate msg model =
    case msg of
        UpdateTitle newTitle ->
            { model | editingName = Just newTitle }

        SaveTitle ->
            case model.editingName of
                Nothing ->
                    model

                Just editingName ->
                    { model | name = editingName, editingName = Nothing }

        UpdateStep name ->
            { model | editingStep = Just <| Step name }

        SaveStep ->
            case model.editingStep of
                Nothing ->
                    model

                Just editingStep ->
                    { model
                        | steps = List.append model.steps [ editingStep ]
                        , editingStep = Nothing
                    }


view : Model -> Html Msg
view model =
    div
        [ class "mt-4" ]
        (case model.mode of
            Run checklistRun ->
                [ viewChecklistRun checklistRun ]

            Create parameters ->
                [ viewChecklistParameters parameters ]

            Browse ->
                [ div []
                    [ button
                        [ onClick CreateChecklist ]
                        [ text "❇️ Add Checklist" ]
                    , viewChecklistList model.checklists model.inProgressList
                    ]
                ]
        )


viewChecklistParameters : NewChecklistParameters -> Html Msg
viewChecklistParameters parameters =
    case parameters.editingName of
        Just name ->
            viewChecklistParametersEmpty name

        Nothing ->
            let
                value_ =
                    case parameters.editingStep of
                        Nothing ->
                            ""

                        Just step ->
                            step.name

                placeholder_ =
                    if List.isEmpty parameters.steps then
                        "First step"

                    else
                        "Next step"
            in
            div
                []
                [ div [ class "text-lg font-medium" ] [ text parameters.name ]
                , div []
                    [ ul [] (List.map (\s -> li [] [ text s.name ]) parameters.steps)
                    ]
                , div
                    []
                    [ input
                        [ type_ "text"
                        , class "mt-2 px-1 py-2 italic border w-full"
                        , placeholder placeholder_

                        -- autofocus doesn't work on the first render (i.e.
                        -- first new step), but when this node is re-rendered
                        -- in the DOM the second time it's fine...
                        -- See also https://github.com/elm/html/issues/186
                        , autofocus True
                        , value value_
                        , onInput (\i -> UpdateChecklist <| UpdateStep i)
                        , onEnter (UpdateChecklist SaveStep)
                        ]
                        []
                    ]
                , div
                    [ class "italic text-sm w-full text-right text-gray-500" ]
                    [ text "Press Enter to add" ]
                , div [ class "text-right w-full mt-2" ]
                    [ viewDiscardChecklist
                    , button
                        [ class "bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 ml-1 rounded"
                        , onClick SaveChecklist
                        ]
                        [ text "Save" ]
                    ]
                ]


viewDiscardChecklist : Html Msg
viewDiscardChecklist =
    button
        [ class "py-2 px-4 border rounded"
        , onClick DiscardChecklist
        ]
        [ text "Cancel" ]


viewChecklistParametersEmpty : String -> Html Msg
viewChecklistParametersEmpty titleValue =
    div
        []
        [ label [] [ text "What's the name of your checklist?" ]
        , input
            [ type_ "text"
            , class "mt-2 px-1 py-2 italic border w-full"
            , placeholder "My awesome checklist"
            , autofocus True
            , value titleValue
            , onInput (\i -> UpdateChecklist <| UpdateTitle i)
            , onEnter (UpdateChecklist SaveTitle)
            ]
            []
        , div
            [ class "text-right mt-2" ]
            [ viewDiscardChecklist ]
        ]


viewChecklistList : List Checklist -> List ChecklistRun -> Html Msg
viewChecklistList checklists inProgressList =
    let
        viewChecklistEntry_ checklist =
            if List.member checklist.uid <| List.map (\run -> run.checklist.uid) inProgressList then
                viewChecklistEntry checklist True

            else
                viewChecklistEntry checklist False
    in
    div []
        [ ul [] (List.map viewChecklistEntry_ checklists)
        ]


viewChecklistEntry : Checklist -> Bool -> Html Msg
viewChecklistEntry checklist inProgress =
    let
        timeString =
            if inProgress then
                "in progress"

            else
                case checklist.lastCompleted of
                    Just completed ->
                        "last completed at " ++ toHHMMSSString completed

                    Nothing ->
                        "never run"
    in
    li
        []
        [ a
            [ href "#", onClick <| Select checklist ]
            [ span [] [ text <| "👉 " ++ checklist.name ]
            , span [ class "text-gray-500 italic" ] [ text <| " " ++ timeString ]
            ]
        ]


isCompleted : ChecklistRun -> Bool
isCompleted checklistRun =
    checklistRun.currentStep >= List.length checklistRun.checklist.steps


viewChecklistRun : ChecklistRun -> Html Msg
viewChecklistRun checklistRun =
    let
        stepsViewData =
            makeViewDataForSteps checklistRun
    in
    div []
        [ div [] (List.map viewStep stepsViewData)
        , div []
            (if isCompleted checklistRun then
                [ div [ class "my-3" ] [ text "All done" ] ]

             else
                []
            )
        , div [ class "mt-4 clearfix" ]
            (if isCompleted checklistRun then
                [ button
                    [ class "float-left py-2 px-4 border rounded"
                    , onClick BackHome
                    ]
                    [ text "Back" ]
                ]

             else
                [ button
                    [ class "float-left py-2 px-4 border rounded"
                    , onClick BackHome
                    ]
                    [ text "Back" ]
                , button
                    [ class "float-left py-2 px-4 border rounded ml-1"
                    , onClick (Discard checklistRun.checklist)
                    ]
                    [ text "Discard" ]
                ]
            )
        ]


viewBack : Html Msg
viewBack =
    a [ class "underline", onClick BackHome ] [ text "Back" ]


toHHMMSSString : Time.Posix -> String
toHHMMSSString time =
    let
        hour =
            String.fromInt (Time.toHour Time.utc time)

        minute =
            String.fromInt (Time.toMinute Time.utc time)

        second =
            String.fromInt (Time.toSecond Time.utc time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second ++ " UTC"


type alias ChecklistStepViewData =
    { text : String
    , completed : Bool
    , active : Bool
    }


viewStep : ChecklistStepViewData -> Html Msg
viewStep viewData =
    div []
        -- TODO: using viewData.text for the id might result in
        -- inconsistencies if there are multiple steps with the same name
        [ input
            [ type_ "checkbox"
            , class "mr-2"
            , checked viewData.completed
            , disabled <| not viewData.active
            , id viewData.text
            , onCheck (\_ -> MoveToNext)
            ]
            []

        -- TODO: is it safe to always move to next? what if the user somehow
        -- manages to have the checkbox checked? should that be a
        -- MoveToPrevious?
        , label
            [ for viewData.text ]
            [ text viewData.text ]
        ]


makeViewDataForSteps : ChecklistRun -> List ChecklistStepViewData
makeViewDataForSteps checklistRun =
    List.indexedMap
        (\index value -> makeViewModel value index checklistRun.currentStep)
        checklistRun.checklist.steps


makeViewModel : Step -> Int -> Int -> ChecklistStepViewData
makeViewModel step index currentIndex =
    if index < currentIndex then
        { text = step.name, completed = True, active = False }

    else if index == currentIndex then
        { text = step.name, completed = False, active = True }

    else
        { text = step.name, completed = False, active = False }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)
