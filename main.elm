module Main exposing (main)

import List
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (send, get, Error(..))
import Json.Encode
import Json.Decode
import Tuple exposing (first, second)


main =
    program
        { init = { name = "", owed = 0, bill = defaultItems } ! [] 
        , view = view
        , update = update
        , subscriptions = \x -> Sub.none
        }


type alias Model =
    { name : String, owed : Float, bill : List Item }


type Claim
    = Unclaimed
    | ClaimedBy String


type alias Item =
    { description : String, price : Float, claim : Claim }


type Msg
    = ToggleClaim String
    | NoOp

update : Msg -> Model -> (Model, Cmd msg)
update x y = y ! []

-- encodeBill =
-- decodeBill =
--
-- urlBase =
--     "https://pebble-timetracking.firebaseio.com/bill"
--
--
--putRequest bill =
--    Http.request
--        { method = "PUT"
--        , headers = []
--        , url = urlBase ++ ".json"
--        , body = Http.jsonBody (encodeBill bill)
--        , expect = Http.expectJson decodeActivities
--        , timeout = Nothing
--        , withCredentials = False
--        }


defaultItems =
    List.map (\x -> (Item (first x) (second x) Unclaimed)) items


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


view : Model -> Html Msg
view m =
    div [] 
    (List.map itemRow m.bill)


itemRow : Item -> Html Msg
itemRow item =
    let
        c =
            case item.claim of
                Unclaimed ->
                    False

                ClaimedBy _ ->
                    True
    in
        div []
            [ input
                [ type_ "checkbox"
                , checked c
                , onClick (ToggleClaim item.description)
                ]
                []
            , text "Mine"
            ]
