module Projekt exposing (..)

import Browser
import Html exposing (Html, div, button, text, h1)
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
                (newTestModel, cmd) =
                    Test.update (Test.ChangePlot "scatter") model.testModel
            in
            ( { model | currentPlot = ShowScatter, testModel = newTestModel }
            , Cmd.map TestMsg cmd
            )

        ShowParallelMsg ->
            let
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
    div [ HtmlAttr.style "font-family" "Arial, sans-serif"
        , HtmlAttr.style "display" "flex"
        , HtmlAttr.style "flex-direction" "column"
        , HtmlAttr.style "align-items" "center"
        , HtmlAttr.style "margin" "20px"
        , HtmlAttr.style "min-height" "100vh"
        , HtmlAttr.style "background-color" "#f4f4f9"
        ]
        [ -- Title
          h1 [ HtmlAttr.style "font-size" "36px"
              , HtmlAttr.style "font-weight" "bold"
              , HtmlAttr.style "margin-bottom" "30px"
              , HtmlAttr.style "text-align" "center"
              , HtmlAttr.style "color" "#333"
              ]
              [ text "Health and Lifestyle" ]
          
          -- Buttons for plot selection
        , div [ HtmlAttr.style "margin-bottom" "20px" ]
            [ styledButton "#4CAF50" "Baum" ShowBaumMsg
            , styledButton "#2196F3" "Scatterplot" ShowScatterMsg
            , styledButton "#FF5722" "Mehrdimensionale" ShowParallelMsg
            ]

          -- Diagram container
        , div [ HtmlAttr.style "width" "90%"
              , HtmlAttr.style "max-width" "1200px"
              , HtmlAttr.style "background-color" "white"
              , HtmlAttr.style "padding" "20px"
              , HtmlAttr.style "border-radius" "12px"
              , HtmlAttr.style "box-shadow" "0 4px 10px rgba(0,0,0,0.1)"
              , HtmlAttr.style "margin-bottom" "50px"
              ]
            [ case model.currentPlot of
                ShowBaum ->
                    Html.map BaumMsg (Baum.graphView model.baumModel)

                ShowScatter ->
                    Html.map TestMsg (Test.view model.testModel)

                ShowParallel ->
                    Html.map TestMsg (Test.view model.testModel)
            ]
        ]


-- BUTTON STYLE HELPER

styledButton : String -> String -> Msg -> Html Msg
styledButton color label msg =
    button
        [ HtmlEvents.onClick msg
        , HtmlAttr.style "padding" "10px 22px"
        , HtmlAttr.style "margin-right" "10px"
        , HtmlAttr.style "border" "none"
        , HtmlAttr.style "border-radius" "8px"
        , HtmlAttr.style "cursor" "pointer"
        , HtmlAttr.style "font-weight" "600"
        , HtmlAttr.style "font-size" "14px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "background-color" color
        , HtmlAttr.style "box-shadow" "0 4px 8px rgba(0,0,0,0.1)"
        , HtmlAttr.style "transition" "all 0.2s ease"
        , HtmlAttr.style "outline" "none"
        ]
        [ text label ]


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
