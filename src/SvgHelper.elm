module SvgHelper exposing
    ( ViewBox
    , cellSize
    , defs
    , hexOffset
    , href
    , viewBox
    )

import Html.Attributes
import Svg
    exposing
        ( Svg
        , circle
        , polygon
        , svg
        )
import Svg.Attributes as A


type alias ViewBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


cellSize : Float
cellSize =
    33


viewBox : ViewBox -> String
viewBox { x, y, width, height } =
    String.join " " (List.map String.fromFloat [ x, y, width, height ])


hexOffset : Float
hexOffset =
    sqrt 3 / 2


hexPoints =
    List.concat hexagonPoints
        |> List.map String.fromFloat
        |> String.join " "



--   .map(({ x, y }) => `${(x * cellSize) / 2},${(y * cellSize) / 2}`)
--   .join(' ');


hexagonPoints =
    List.map (\( y, x ) -> [ (x * cellSize) / 2, (y * cellSize) / 2 ])
        [ ( 0, 1 )
        , ( 0.5, hexOffset + 1 )
        , ( 1.5, hexOffset + 1 )
        , ( 2, 1 )
        , ( 1.5, -hexOffset + 1 )
        , ( 0.5, -hexOffset + 1 )
        ]


squarePoints =
    let
        gap =
            2

        points =
            [ [ gap, gap ]
            , [ gap, cellSize - gap ]
            , [ cellSize - gap, cellSize - gap ]
            , [ cellSize - gap, gap ]
            ]
    in
    List.concat points
        |> List.map String.fromFloat
        |> String.join " "


defs : Svg msg
defs =
    let
        csHalf =
            String.fromFloat (cellSize / 2)
    in
    svg [ Html.Attributes.style "visibility" "hidden" ]
        [ Svg.defs []
            [ circle
                [ A.id "circle"
                , A.cx csHalf
                , A.cy csHalf
                , A.r (String.fromFloat ((cellSize / 2) - 2.0))
                ]
                []
            , polygon
                [ A.id "HEX"
                , A.fillOpacity "1"
                , A.strokeWidth "0"
                , A.width (String.fromFloat cellSize)
                , A.height (String.fromFloat cellSize)
                , A.points hexPoints
                ]
                []
            , polygon
                [ A.id "SQUARE"
                , A.fillOpacity "1"
                , A.strokeWidth "0"
                , A.width (String.fromFloat cellSize)
                , A.height (String.fromFloat cellSize)
                , A.points squarePoints
                ]
                []
            ]
        ]



-- TODO change to normal href when elm supports it


href =
    A.xlinkHref
