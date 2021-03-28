module Takuzu exposing (..)

---- CREDITS :
-- Takuzu (Binary Sudoku in Elm) with Random Game Generator and Validator
-- Made in Elm by Ni'Gere Epps

import Array
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Debug
import Html exposing (Html, button, div)
import Html.Attributes as Attr
import Json.Decode as Decode
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Task



---- CAN ADJUST THE FOLLOWING : NUMCELLS (4 or 6)


numCells =
    -- Starting number of rows and columns for testing ie 4 (= 4x4) or 6 (= 6x6)
    4



-- MAIN ----------------------------------------------------


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, Task.perform GotViewport Browser.Dom.getViewport )


type alias Flags =
    ()


type alias Point =
    { x : Int, y : Int }



-- MODEL ---------------------------------------------------


type alias Board =
    Array.Array (Array.Array Int)


type alias Model =
    { curBoard : Board
    , startBoard : Board
    , windowSize : ( Int, Int )
    }


upCur : Model -> Board -> Model
upCur { curBoard, startBoard, windowSize } board =
    -- Updates the current board of a given model
    { curBoard = board
    , startBoard = startBoard
    , windowSize = windowSize
    }


upSize : Model -> ( Int, Int ) -> Model
upSize { curBoard, startBoard, windowSize } newsize =
    -- Updates the window size of a given model
    { curBoard = curBoard
    , startBoard = startBoard
    , windowSize = newsize
    }


initModel : Model
initModel =
    if numCells == 4 then
        let
            board =
                Array.fromList
                    (List.map (\row -> Array.fromList row)
                        sample4x4
                    )
        in
        { curBoard = board
        , startBoard = board
        , windowSize = ( 1366, 654 )
        }

    else
        let
            board =
                Array.fromList
                    (List.map (\row -> Array.fromList row)
                        sample6x6
                    )
        in
        { curBoard = board
        , startBoard = board
        , windowSize = ( 1366, 654 )
        }


sample4x4 =
    [ [ 9, 9, 9, 1 ]
    , [ 0, 9, 9, 9 ]
    , [ 0, 0, 9, 9 ]
    , [ 9, 9, 9, 9 ]
    ]


blank4x4 =
    [ [ 9, 9, 9, 9 ]
    , [ 9, 9, 9, 9 ]
    , [ 9, 9, 9, 9 ]
    , [ 9, 9, 9, 9 ]
    ]


blank4b =
    Array.fromList (List.map (\rowz -> Array.fromList rowz) blank4x4)


blank6x6 =
    [ [ 9, 9, 9, 9, 9, 9 ]
    , [ 9, 9, 9, 9, 9, 9 ]
    , [ 9, 9, 9, 9, 9, 9 ]
    , [ 9, 9, 9, 9, 9, 9 ]
    , [ 9, 9, 9, 9, 9, 9 ]
    , [ 9, 9, 9, 9, 9, 9 ]
    ]


blank6b =
    Array.fromList (List.map (\rowz -> Array.fromList rowz) blank6x6)



-- 0 = blue, 1 = red, 9 = empty


sample6x6 =
    [ [ 9, 0, 9, 9, 9, 9 ]
    , [ 1, 9, 1, 0, 9, 9 ]
    , [ 1, 9, 9, 9, 9, 9 ]
    , [ 9, 9, 9, 0, 9, 9 ]
    , [ 1, 9, 0, 9, 9, 9 ]
    , [ 9, 9, 9, 0, 9, 9 ]
    ]


bSize =
    --board size in pixels
    360



-- SUBSCRIPTIONS -------------------------------------------


type Msg
    = Click Point
    | Reset
    | New4
    | Random4 (List Int)
    | New6
    | Random6 (List Int)
    | Reveal (List Int)
    | NewWindowSize ( Int, Int )
    | GotViewport Viewport


pointToMsg : Point -> Msg
pointToMsg point =
    Click point


clickDecoder : Decode.Decoder Point
clickDecoder =
    Decode.map2
        (\x y -> { x = x, y = y })
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onClick
            (Decode.map pointToMsg clickDecoder)
        , Browser.Events.onResize (\w h -> NewWindowSize ( w, h ))
        ]



-- UPDATE --------------------------------------------------
{--
The following commented code was for a potential random board generator based on actionable tiles.
It generated two actionable tiles (ie two of the same color adjacent to each other) and 1-2 random tiles

type alias RandomBoard =
    { colorx : Int
    , secondtile : ( Int, Int )
    , fourthtile : ( Int, Int )
    , rowcol : ( Int, Int )
    }

boardGenerator : Random.Generator RandomBoard
boardGenerator =
    Random.map4 (\a b c d -> RandomBoard a b c d)
        (Random.int 0 1)
        (Random.pair (Random.int 0 1) (Random.int 0 1))
        (Random.pair (Random.int 0 1) (Random.int 0 1))
        (Random.pair (Random.int 0 3) (Random.int 0 3))


twoTiles : ( Int, Int ) -> Int -> ( Int, Int ) -> Board -> Board
twoTiles ( rownum, colnum ) colorx ( twoCloseFar, direction ) board =
    --Adds two actionable tiles (ie two adjacent of same color) to board
    let
        onetile =
            boardUpdate rownum colnum colorx board
    in
    if direction == 0 then
        --horizontal
        if twoCloseFar == 0 then
            --right next to each other
            if colnum == 3 then
                --edge case
                boardUpdate rownum (colnum - 1) colorx onetile

            else
                boardUpdate rownum (colnum + 1) colorx onetile

        else if colnum >= 2 then
            --edge case
            boardUpdate rownum (colnum - 2) colorx onetile

        else
            boardUpdate rownum (colnum + 2) colorx onetile

    else
    --vertical
    if
        twoCloseFar == 0
    then
        --right on top of each other
        if rownum == 3 then
            --edge case
            boardUpdate (rownum - 1) colnum colorx onetile

        else
            boardUpdate (rownum + 1) colnum colorx onetile

    else if rownum >= 2 then
        --edge case
        boardUpdate (rownum - 2) colnum colorx onetile

    else
        boardUpdate (rownum + 2) colnum colorx onetile



generate4x4 : RandomBoard -> Model
generate4x4 record =
    let
        ( twoCloseFar, direction ) =
            record.secondtile

        ( fourCloseFar, direction2 ) =
            record.fourthtile

        -- if twoCloseFar = 0 then generates two adjacent, otherwise two one apart
        -- if direction = 0 then horizontal, otherwise vertical
        ( rownum, colnum ) =
            record.rowcol

        offset =
            2
    in
    (if twoCloseFar == 1 && fourCloseFar == 1 && direction /= direction2 then
        --edge case for overlap
        twoTiles record.rowcol 0 ( 0, direction ) blank4b

     else
        twoTiles record.rowcol 0 record.secondtile blank4b
    )
        |> (if rownum < 2 then
                if colnum < 2 then
                    twoTiles ( rownum + offset, colnum + offset ) 1 record.fourthtile
                        >> boardUpdate 3 0 1
                        >> boardUpdate (rownum + 1) (colnum + 1) 0

                else
                    twoTiles ( rownum + offset, colnum - offset ) 1 record.fourthtile
                        >> boardUpdate 3 3 1
                        >> boardUpdate (rownum + 1) (colnum - 1) 0

            else if colnum < 2 then
                twoTiles ( rownum - offset, colnum + offset ) 1 record.fourthtile
                    >> boardUpdate 0 0 1
                    >> boardUpdate (rownum - 1) (colnum + 1) 0

            else
                twoTiles ( rownum - offset, colnum - offset ) 1 record.fourthtile
                    >> boardUpdate 0 3 1
                    >> boardUpdate (rownum - 1) (colnum - 1) 0
           )
        |> boardToNew

--}


list4Generator : Random.Generator (List Int)
list4Generator =
    Random.list 16 (Random.int 0 1)


list6Generator : Random.Generator (List Int)
list6Generator =
    Random.list 36 (Random.int 0 1)


reveal4Generator : Random.Generator (List Int)
reveal4Generator =
    Random.list 36 (Random.weighted ( 40, 1 ) [ ( 60, 0 ) ])


reveal6Generator : Random.Generator (List Int)
reveal6Generator =
    Random.list 36 (Random.weighted ( 30, 1 ) [ ( 70, 0 ) ])


generateListed4x4 xs rowacc colacc board =
    case xs of
        [] ->
            board

        x :: rest ->
            let
                newboard =
                    boardUpdate rowacc colacc x board

                listboard =
                    boardToList newboard
            in
            if checkValidv2 listboard then
                if colacc == 3 then
                    generateListed4x4 rest (rowacc + 1) 0 newboard

                else
                    generateListed4x4 rest rowacc (colacc + 1) newboard

            else
                let
                    newboard2 =
                        boardUpdate rowacc colacc (opposite x) board
                in
                if colacc == 3 then
                    generateListed4x4 rest (rowacc + 1) 0 newboard2

                else
                    generateListed4x4 rest rowacc (colacc + 1) newboard2


generateListed6x6 xs rowacc colacc board =
    case xs of
        [] ->
            board

        x :: rest ->
            let
                newboard =
                    boardUpdate rowacc colacc x board

                listboard =
                    boardToList newboard
            in
            if checkValidv2 listboard then
                if colacc == 5 then
                    generateListed6x6 rest (rowacc + 1) 0 newboard

                else
                    generateListed6x6 rest rowacc (colacc + 1) newboard

            else
                let
                    newboard2 =
                        boardUpdate rowacc colacc (opposite x) board
                in
                if colacc == 5 then
                    generateListed6x6 rest (rowacc + 1) 0 newboard2

                else
                    generateListed6x6 rest rowacc (colacc + 1) newboard2


checkValidv2 : List (List Int) -> Bool
checkValidv2 xxs =
    noTripleRows xxs
        -- Not 3 of the same color adjacent
        && noTripleRows (transpose xxs)
        && notTooMany xxs (List.length xxs)
        -- Equal number of reds and blues
        && notTooMany (transpose xxs) (List.length xxs)


gen4x4 list =
    generateListed4x4 list 0 0 blank4b


gen6x6 list =
    generateListed6x6 list 0 0 blank6b


opposite cell =
    if cell == 0 then
        1

    else
        0


boardToNew : Board -> Model
boardToNew board =
    { curBoard = board
    , startBoard = board
    , windowSize = ( 1366, 654 )
    }


boardUpdate : Int -> Int -> Int -> Board -> Board
boardUpdate rowIdx colIdx colorn board =
    let
        row =
            Maybe.withDefault Array.empty
                (Array.get rowIdx board)

        newrow =
            Array.set colIdx colorn row
    in
    Array.set rowIdx newrow board


newCell : Int -> Int
newCell old =
    if old == 0 then
        1

    else if old == 1 then
        9

    else
        0


getXIdx : Model -> Int -> Int
getXIdx model coord =
    let
        xOffset =
            (toFloat (Tuple.first model.windowSize) / 2) - (bSize / 2)

        sq =
            bSize // Array.length model.startBoard
    in
    (coord - round xOffset) // sq


getYIdx : Model -> Int -> Int
getYIdx model coord =
    let
        yOffset =
            (toFloat (Tuple.second model.windowSize) / 2) - (bSize / 2)

        sq =
            bSize // Array.length model.startBoard
    in
    (coord - round yOffset) // sq


boardToList : Board -> List (List Int)
boardToList arr =
    Array.toList (Array.map (\row -> Array.toList row) arr)


checkFull : List (List Int) -> Bool
checkFull xxs =
    case xxs of
        [] ->
            True

        x :: xs ->
            if List.member 9 x then
                False

            else
                checkFull xs


transpose : List (List a) -> List (List a)
transpose xxs =
    List.foldr (List.map2 (::)) (List.repeat (List.length xxs) []) xxs


checkValid : List (List Int) -> Bool
checkValid xxs =
    noTripleRows xxs
        -- Not 3 of the same color adjacent
        && noTripleRows (transpose xxs)
        && notTooMany xxs (List.length xxs)
        -- Equal number of reds and blues
        && notTooMany (transpose xxs) (List.length xxs)
        && noDuplicateRows xxs
        -- No duplicate rows or columns
        && noDuplicateRows (transpose xxs)


notTooMany : List (List Int) -> Int -> Bool
notTooMany xxs num =
    case xxs of
        [] ->
            True

        x :: xs ->
            (List.length (List.filter (\item -> item == 0) x) <= num // 2)
                && (List.length (List.filter (\item -> item == 1) x) <= num // 2)
                && notTooMany xs num


noDuplicate : List Int -> List (List Int) -> Bool
noDuplicate row1 =
    List.foldr
        (\row2 -> (&&) (row1 /= row2))
        True


noDuplicateRows : List (List Int) -> Bool
noDuplicateRows xxs =
    case xxs of
        [] ->
            True

        x :: xs ->
            noDuplicate x xs && noDuplicateRows xs


noTripleRow : List Int -> Bool
noTripleRow xs =
    case xs of
        [] ->
            True

        x1 :: x2 :: x3 :: rest ->
            if x1 /= 9 && x1 == x2 && x1 == x3 then
                False

            else
                noTripleRow (x2 :: x3 :: rest)

        x :: rest ->
            noTripleRow rest


noTripleRows : List (List Int) -> Bool
noTripleRows xxs =
    case xxs of
        [] ->
            True

        x :: xs ->
            if noTripleRow x then
                noTripleRows xs

            else
                False


revealBoard : List Int -> Int -> Int -> Board -> Board
revealBoard xs rowacc colacc board =
    case xs of
        [] ->
            board

        x :: rest ->
            let
                numSquares =
                    Array.length board - 1
            in
            if colacc == numSquares then
                --move to next row
                if x == 0 then
                    let
                        newboard =
                            boardUpdate rowacc colacc 9 board
                    in
                    revealBoard rest (rowacc + 1) 0 newboard

                else
                    revealBoard rest (rowacc + 1) 0 board

            else if x == 0 then
                let
                    newboard =
                        boardUpdate rowacc colacc 9 board
                in
                revealBoard rest rowacc (colacc + 1) newboard

            else
                revealBoard rest rowacc (colacc + 1) board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( upCur model model.startBoard, Cmd.none )

        NewWindowSize size ->
            ( upSize model size, Cmd.none )

        GotViewport viewport ->
            ( upSize model ( round viewport.viewport.width, round viewport.viewport.height ), Cmd.none )

        New4 ->
            ( model, Random.generate Random4 list4Generator )

        New6 ->
            ( model, Random.generate Random6 list6Generator )

        Random4 randoms4 ->
            if checkValid (boardToList (gen4x4 randoms4)) == False then
                ( model, Random.generate Random4 list4Generator )

            else
                ( gen4x4 randoms4 |> boardToNew, Random.generate Reveal reveal4Generator )

        Random6 randoms6 ->
            if checkValid (boardToList (gen6x6 randoms6)) == False then
                ( model, Random.generate Random6 list6Generator )

            else
                ( gen6x6 randoms6 |> boardToNew, Random.generate Reveal reveal4Generator )

        Reveal revealList ->
            ( revealBoard revealList 0 0 model.curBoard |> boardToNew, Cmd.none )

        Click pt ->
            let
                rownum =
                    getYIdx model pt.y

                colnum =
                    getXIdx model pt.x

                initRow =
                    Maybe.withDefault Array.empty
                        (Array.get rownum model.startBoard)

                initCell =
                    Maybe.withDefault -1
                        (Array.get colnum initRow)
            in
            if initCell == 0 || initCell == 1 then
                --dont change starting tiles
                ( model, Cmd.none )

            else
                let
                    row =
                        Maybe.withDefault Array.empty
                            (Array.get rownum model.curBoard)

                    newNum =
                        newCell
                            (Maybe.withDefault -1
                                (Array.get colnum row)
                            )

                    newrow =
                        Array.set colnum newNum row
                in
                let
                    newboard =
                        Array.set rownum newrow model.curBoard
                in
                ( upCur model newboard, Cmd.none )



-- VIEW ----------------------------------------------------


color : Int -> String
color cell =
    if cell == 0 then
        "red"

    else if cell == 1 then
        "blue"

    else
        "gray"


rowDraw xs row =
    let
        sq =
            bSize // Array.length xs
    in
    Array.indexedMap
        (\col cell ->
            rect
                [ x (String.fromInt (sq * col))
                , y (String.fromInt (sq * row))
                , width (String.fromInt sq)
                , height (String.fromInt sq)
                , fill (color cell)
                , rx "15"
                , stroke "white"
                , strokeWidth "0.5%"
                ]
                []
        )
        xs


boardDraw : Model -> Html Msg
boardDraw model =
    let
        xOffset =
            (toFloat (Tuple.first model.windowSize) / 2) - (bSize / 2)

        yOffset =
            (toFloat (Tuple.second model.windowSize) / 2) - (bSize / 2)
    in
    svg [ x (String.fromFloat xOffset), y (String.fromFloat yOffset) ]
        (Array.toList
            (Array.foldl Array.append
                Array.empty
                (Array.indexedMap
                    (\acc row -> rowDraw row acc)
                    model.curBoard
                )
            )
        )


attrMap styles =
    List.map (\( k, v ) -> Attr.style k v) styles


view : Model -> Html Msg
view model =
    let
        xOffset =
            (toFloat (Tuple.first model.windowSize) / 2) - (bSize / 2)

        yOffset =
            (toFloat (Tuple.second model.windowSize) / 2) - (bSize / 2)

        listmodel =
            boardToList model.curBoard

        styles =
            []

        btnStyles =
            [ ( "position", "relative" )
            , ( "text-align", "center" )
            , ( "background-color", "#1DA70C" )
            , ( "border", "none" )
            , ( "color", "white" )
            , ( "padding", "12px 18px" )
            , ( "font-size", "16px" )
            , ( "margin", "4px 6px" )
            , ( "border-radius", "6px" )
            ]

        headerStyles =
            [ ( "text-align", "center" )
            , ( "position", "fixed" )
            , ( "left", "50%" )
            , ( "transform", "translate(-50%, -50%)" )
            , ( "color", "blue" )
            , ( "text-shadow", "2px 2px 5px red" )
            , ( "font-size", "50px" )
            ]

        rulesStyles =
            [ ( "text-align", "center" )
            , ( "position", "fixed" )
            , ( "width", "80%" )
            , ( "left", "50%" )
            , ( "transform", "translate(-50%, 40px)" )
            ]

        fullStyles =
            [ ( "text-align", "center" )
            , ( "position", "fixed" )
            , ( "left", "50%" )
            , ( "transform", "translate(-50%, 60px)" )
            , ( "font-size", "20px" )
            , ( "font-family", "Impact, fantasy" )
            ]

        btnDivStyles =
            [ ( "position", "fixed" )
            , ( "text-align", "center" )
            , ( "width", "100%" )
            , ( "color", "blue" )
            ]

        br =
            Html.br [] []

        title =
            Html.h1 (attrMap headerStyles)
                [ text "Takuzu" ]

        rules =
            Html.h4 (attrMap rulesStyles)
                [ text "Rule 1: There cannot be three (or more) of the same color adjacent to each other."
                , br
                , text "Rule 2: Each row and column must have an equal number of reds and blues."
                , br
                , text "Rule 3: No two rows or no two columns can be identical."
                ]

        draw =
            svg
                [ width (String.fromFloat (bSize + xOffset))
                , height (String.fromFloat (bSize + yOffset))
                ]
                [ boardDraw model ]

        buttons =
            div (attrMap btnDivStyles)
                [ button (onClick Reset :: attrMap btnStyles) [ text "Reset" ]
                , button (onClick New4 :: attrMap btnStyles) [ text "New 4x4 Game" ]
                , button (onClick New6 :: attrMap btnStyles) [ text "New 6x6 Game" ]
                ]
    in
    if checkFull listmodel then
        --all tiles filled
        if checkValid listmodel then
            --valid solution
            let
                full =
                    Html.h2 (attrMap fullStyles)
                        [ text "Congratulations! Your solution is correct!" ]
            in
            div [] [ title, rules, draw, br, full, br, buttons ]

        else
            let
                full =
                    Html.h2 (attrMap fullStyles)
                        [ text "Your solution is incorrect, please try again" ]
            in
            div [] [ title, rules, draw, br, full, br, buttons ]

    else
        div [] [ title, rules, draw, br, br, buttons ]
