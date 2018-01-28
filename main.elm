module Main exposing (main)

import List
import Html exposing (..)


type alias Model =
    { name : String
    , owed : float
    , bill : List ( Bool, String, Float )
    }


init =
    { name = "", owed = 0, bill = defaultItems }


items =
    [ ( "Bone Marrow", 17 )
    , ( "French Fries", 8 )
    , ( "HH Abita Purple Haze", 5 )
    , ( "HHThe Screws Are Loose", 10 )
    , ( "HHThe Screws Are Loose", 10 )
    , ( "HH Craft Lager", 10 )
    , ( "HH Craft Lager", 10 )
    , ( "Outer Space Canoe 16 oz", 8 )
    , ( "HH The Screws are Loose", 5 )
    , ( "French Fries", 8 )
    , ( "Chicken Wings, side of arugala salad", 12 )
    , ( "Open Kitchedn Salad", 2 )
    , ( "Charcuterie platter", 16 )
    , ( "Waygu Burger with fries, medium", 20 )
    , ( "Waygu Burger with salad, white cheddar", 20 )
    , ( "Blue Cheeseburger w/ Salad", 16 )
    , ( "Spicy Burger w/ Fries", 17 )
    , ( "Burger w/ Salad", 16 )
    , ( "HH The Screws are Loose", 5 )
    , ( "Mac and Cheese", 15 )
    , ( "Fish and Chips", 18 )
    , ( "Tossed Salad", 10 )
    , ( "Stout as a Service", 6 )
    , ( "Hef-D 160z", 7.5 )
    ]


defaultItems =
    map (\x -> ( False, x.first, x.second )) items


view : Model -> Html Msg
view m =
    List.map itemRow m.bill
        |> div []


itemRow : ( Bool, String, Float )
itemRow ( paidFor, description, price ) =
    div []
        [ input
            [ type_ "checkbox"
            , checked
            , onClick ToggleAccuracy
            ]
            []
        , text "Mine"
        ]
