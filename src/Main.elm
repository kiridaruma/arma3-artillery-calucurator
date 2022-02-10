module Main exposing (..)

import Browser exposing (Document, document)
import Html exposing (..)
import Html.Attributes exposing (for, id, style)
import Html.Events exposing (onInput)
import Model exposing (..)


type Msg
    = InputFpGrid String
    | InputTargetGrid String
    | InputObserverDirection String
    | InputObserverAdjust String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputFpGrid input ->
            ( { model | fpGrid = Result.withDefault ( "0000", "0000" ) (parseGrid input) }, Cmd.none )

        InputTargetGrid input ->
            ( { model | targetGrid = Result.withDefault ( "0000", "0000" ) (parseGrid input) }, Cmd.none )

        InputObserverDirection input ->
            ( { model | observerDirection = foDirection2Int input }, Cmd.none )

        InputObserverAdjust input ->
            ( { model | observerAdjust = Result.withDefault ( Right 0, Add 0 ) (parseAdjust input) }, Cmd.none )


view : Model -> Document Msg
view m =
    { body = body m
    , title = "arma3 artillery calucurator"
    }


body : Model -> List (Html Msg)
body m =
    [ headerPart
    , inputPart
    , displayPart m
    ]


headerPart : Html Msg
headerPart =
    header [ style "margin" "1rem 10vw" ]
        [ h1 [] [ text "arma3 artillery calucurator" ]
        , p [] [ text "input your arty parameters" ]
        , ul []
            [ li [] [ text "grid must be '1234, 5678' format (8code)" ]
            , li [] [ text "FO's direction must be 'l[eft]100 a[dd]300' format (L/R - left/right, A/D - add/drop)" ]
            ]
        ]


inputPart : Html Msg
inputPart =
    section [ style "margin" "1rem 10vw 2rem" ]
        [ div []
            [ label [ for "fp-grid" ] [ text "fire position: " ]
            , input [ id "fp-grid", onInput InputFpGrid ] []
            ]
        , div []
            [ label [ for "target-grid" ] [ text "target grid: " ]
            , input [ id "target-grid", onInput InputTargetGrid ] []
            ]
        , div []
            [ label [ for "fo-direction" ] [ text "FO direction: " ]
            , input [ id "fo-direction", onInput InputObserverDirection ] []
            ]
        , div []
            [ label [ for "fo-adjust" ] [ text "FO adjust: " ]
            , input [ id "fo-adjust", onInput InputObserverAdjust ] []
            ]
        ]


displayPart : Model -> Html Msg
displayPart m =
    let
        artyParam =
            calcArtyParam
                (Tuple.mapBoth gridCode2int gridCode2int m.fpGrid)
                (Tuple.mapBoth gridCode2int gridCode2int m.targetGrid)

        ( adjustX, adjustY ) =
            rotate
                (adjust2Vector m.observerAdjust)
                (foDirection2rotate m.observerDirection)

        adjustedGrid =
            ( (gridCode2int <| Tuple.first m.targetGrid) + (adjustX // 10)
            , (gridCode2int <| Tuple.second m.targetGrid) + (adjustY // 10)
            )

        adjustedParam =
            calcArtyParam
                (Tuple.mapBoth gridCode2int gridCode2int m.fpGrid)
                adjustedGrid
    in
    section [ style "margin" "1rem 10vw 2rem" ]
        [ p [ style "text-decoration" "underline" ]
            [ text ("your fire position is (" ++ grid2string m.fpGrid ++ ")") ]
        , p [ style "text-decoration" "underline" ]
            [ text ("target grid is (" ++ grid2string m.targetGrid ++ ")") ]
        , p [ style "color" "#b71c1c", style "text-decoration" "underline" ]
            [ text <|
                "arty parameter is (direction:"
                    ++ String.fromInt artyParam.direction
                    ++ ", range:"
                    ++ String.fromInt artyParam.range
                    ++ ")"
            ]
        , p [ style "text-decoration" "underline" ]
            [ text ("FO's direction is " ++ int2FoDirectionCode m.observerDirection ++ "") ]
        , p [ style "text-decoration" "underline" ]
            [ text ("FO's adjust is " ++ adjust2String m.observerAdjust ++ "") ]
        , p [ style "color" "#b71c1c", style "text-decoration" "underline" ]
            [ text <|
                "adjusted parameter is (direction:"
                    ++ String.fromInt adjustedParam.direction
                    ++ ", range:"
                    ++ String.fromInt adjustedParam.range
                    ++ ")"
            ]
        ]


main : Program () Model Msg
main =
    document
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
