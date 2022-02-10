module Model exposing (..)

import Parser exposing (..)


initModel : Model
initModel =
    { fpGrid = ( "0000", "0000" )
    , targetGrid = ( "0000", "0000" )
    , observerDirection = 0
    , observerAdjust = ( Right 0, Add 0 )
    }


type alias Model =
    { fpGrid : Grid
    , targetGrid : Grid
    , observerDirection : Int
    , observerAdjust : Adjust
    }


type alias Grid =
    ( String, String )


type alias Adjust =
    ( HorizontalAdjust, VerticalAdjust )


type HorizontalAdjust
    = Right Int
    | Left Int


type VerticalAdjust
    = Drop Int
    | Add Int


type alias ArtyParams =
    { direction : Int
    , range : Int
    }


grid2string : Grid -> String
grid2string grid =
    let
        fst =
            Tuple.first grid

        scd =
            Tuple.second grid
    in
    fst ++ ", " ++ scd


parseGrid : String -> Result (List DeadEnd) Grid
parseGrid =
    run <|
        succeed Tuple.pair
            |. spaces
            |= ((getChompedString <| chompWhile Char.isDigit)
                    |> andThen
                        (\str ->
                            if String.length str == 4 then
                                succeed str

                            else
                                problem "grid length must be 8"
                        )
               )
            |. spaces
            |. symbol ","
            |. spaces
            |= ((getChompedString <| chompWhile Char.isDigit)
                    |> andThen
                        (\str ->
                            if String.length str == 4 then
                                succeed str

                            else
                                problem "grid length must be 8"
                        )
               )
            |. spaces
            |. end


gridCode2int : String -> Int
gridCode2int str =
    let
        parser =
            succeed identity
                |. chompWhile ((==) '0')
                |= int
    in
    run parser str
        |> Result.withDefault 0


calcArtyParam : ( Int, Int ) -> ( Int, Int ) -> ArtyParams
calcArtyParam ( fpx, fpy ) ( tx, ty ) =
    let
        x =
            toFloat <| tx * 10 - fpx * 10

        y =
            toFloat <| ty * 10 - fpy * 10

        ( r, theta ) =
            toPolar ( x, y )
    in
    { direction = modBy 6400 (ceiling <| (2 * pi - theta) * 3200 / pi + 1600), range = ceiling <| r }


foDirection2Int : String -> Int
foDirection2Int str =
    let
        parsedInt =
            gridCode2int str
    in
    if parsedInt <= 6400 then
        parsedInt

    else
        0


int2FoDirectionCode : Int -> String
int2FoDirectionCode i =
    let
        str =
            String.fromInt i
    in
    case String.length str of
        4 ->
            str

        3 ->
            "0" ++ str

        2 ->
            "00" ++ str

        1 ->
            "000" ++ str

        _ ->
            "0000"


parseAdjust : String -> Result (List DeadEnd) Adjust
parseAdjust =
    let
        horizontalAdjustParser =
            oneOf
                [ succeed Left |. symbol "l"
                , succeed Left |. symbol "left"
                , succeed Left |. symbol "L"
                , succeed Left |. symbol "Left"
                , succeed Right |. symbol "r"
                , succeed Right |. symbol "right"
                , succeed Right |. symbol "R"
                , succeed Right |. symbol "Right"
                ]
                |= int

        verticalAdjustParser =
            oneOf
                [ succeed Drop |. symbol "d"
                , succeed Drop |. symbol "drop"
                , succeed Drop |. symbol "D"
                , succeed Drop |. symbol "Drop"
                , succeed Add |. symbol "a"
                , succeed Add |. symbol "add"
                , succeed Add |. symbol "A"
                , succeed Add |. symbol "Add"
                ]
                |= int
    in
    run <|
        (succeed Tuple.pair
            |. spaces
            |= horizontalAdjustParser
            |. spaces
            |= verticalAdjustParser
            |. spaces
        )


adjust2String : Adjust -> String
adjust2String ( horizontal, vertical ) =
    let
        horizontalStr =
            case horizontal of
                Left i ->
                    "Left " ++ String.fromInt i

                Right i ->
                    "Right " ++ String.fromInt i

        verticalStr =
            case vertical of
                Add i ->
                    "Add " ++ String.fromInt i

                Drop i ->
                    "Drop " ++ String.fromInt i
    in
    horizontalStr ++ " ," ++ verticalStr


adjust2Vector : Adjust -> ( Int, Int )
adjust2Vector ( h, v ) =
    let
        horizontal =
            case h of
                Left n ->
                    n * -1

                Right n ->
                    n

        vertical =
            case v of
                Drop n ->
                    n * -1

                Add n ->
                    n
    in
    ( horizontal, vertical )


foDirection2rotate : Int -> Float
foDirection2rotate n =
    (toFloat <| 6400 - n) * (2 * pi) / 6400


rotate : ( Int, Int ) -> Float -> ( Int, Int )
rotate ( x, y ) theta =
    ( ceiling <| toFloat x * cos theta - toFloat y * sin theta, ceiling <| toFloat x * sin theta + toFloat y * cos theta )
