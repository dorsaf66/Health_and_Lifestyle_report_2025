module Projekt exposing (..)

import Browser
import Html exposing (Html, div, button, text)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Baum
import Test


-- MODEL

type PlotType = ShowBaum | ShowScatter | ShowParallel

type alias Model =
    { currentPlot : PlotType
    , baumModel : Baum.Model
    , testModel : Test.Model
    }


-- MSG

type Msg
    = ShowBaumMsg
    | ShowScatterMsg
    | ShowParallelMsg
    | BaumMsg Baum.Msg
    | TestMsg Test.Msg


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    let
        (baumInit, baumCmd) = Baum.init ()
        (testInit, testCmd) = Test.init ()
    in
    ( { currentPlot = ShowBaum
      , baumModel = baumInit
      , testModel = testInit
      }
    , Cmd.batch
        [ Cmd.map BaumMsg baumCmd
        , Cmd.map TestMsg testCmd
        ]
    )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowBaumMsg ->
            ( { model | currentPlot = ShowBaum }, Cmd.none )

        ShowScatterMsg ->
            let
                -- Trigger Test.ChangePlot "scatter"
                (newTestModel, cmd) =
                    Test.update (Test.ChangePlot "scatter") model.testModel
            in
            ( { model | currentPlot = ShowScatter, testModel = newTestModel }
            , Cmd.map TestMsg cmd
            )

        ShowParallelMsg ->
            let
                -- Trigger Test.ChangePlot "parallel"
                (newTestModel, cmd) =
                    Test.update (Test.ChangePlot "parallel") model.testModel
            in
            ( { model | currentPlot = ShowParallel, testModel = newTestModel }
            , Cmd.map TestMsg cmd
            )

        BaumMsg bMsg ->
            let
                (newBaumModel, bCmd) = Baum.update bMsg model.baumModel
            in
            ( { model | baumModel = newBaumModel }, Cmd.map BaumMsg bCmd )

        TestMsg tMsg ->
            let
                (newTestModel, tCmd) = Test.update tMsg model.testModel
            in
            ( { model | testModel = newTestModel }, Cmd.map TestMsg tCmd )


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [ HtmlAttr.style "margin-bottom" "20px" ]
            [ button [ HtmlEvents.onClick ShowBaumMsg, HtmlAttr.style "margin-right" "10px" ] [ text "Baum" ]
            , button [ HtmlEvents.onClick ShowScatterMsg, HtmlAttr.style "margin-right" "10px" ] [ text "Scatterplot" ]
            , button [ HtmlEvents.onClick ShowParallelMsg ] [ text "Parallel" ]
            ]
        , case model.currentPlot of
            ShowBaum ->
                Html.map BaumMsg (Baum.graphView model.baumModel)

            ShowScatter ->
                Html.map TestMsg (Test.view model.testModel)

            ShowParallel ->
                Html.map TestMsg (Test.view model.testModel)
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Baum.subscriptions model.baumModel
        |> Sub.map BaumMsg


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
