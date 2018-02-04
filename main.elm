module Main exposing (main)

import List
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (send, get, Error(..))
import Json.Encode
import Json.Decode
import Tuple exposing (first, second)
import Maybe exposing (Maybe(..))
import Material
import Material.Options as Options
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color
import Material.List as Lists
import Material.Toggles as Toggles


main =
    program
        { init = { name = "", owed = 0, bill = defaultDict, mdl = Material.model } ! []
        , view = view
        , update = update
        , subscriptions = \x -> Sub.none
        }


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


defaultDict =
    List.map (\x -> ( first x, (Item (first x) (second x) Unclaimed) )) items
        |> Dict.fromList


type alias Model =
    { name : String, owed : Float, bill : Dict String Item, mdl : Material.Model }


type Claim
    = Unclaimed
    | ClaimedBy String


type alias Item =
    { description : String, price : Float, claim : Claim }


type Msg
    = ToggleClaim String
    | NoOp
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ToggleClaim desc ->
            let
                replace oldItem =
                    case oldItem of
                        -- I think there's an Elm default to replace this
                        Nothing ->
                            Nothing

                        Just item ->
                            --This should probably be two helpers
                            Just
                                { item
                                    | claim =
                                        case item.claim of
                                            Unclaimed ->
                                                ClaimedBy model.name

                                            ClaimedBy _ ->
                                                Unclaimed
                                }
            in
                { model | bill = Dict.update desc replace model.bill } ! []

        _ ->
            model ! []



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


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ h2 [ style [ ( "padding", "2rem" ) ] ] [ text "Bill" ] ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewBody model ]
            }


viewBody : Model -> Html Msg
viewBody model =
    let
        itemRow item =
            case item.claim of
                Unclaimed ->
                    Lists.ul []
                        [ Lists.li []
                            [ Lists.content [] [ text item.description ]
                            , Lists.content2 []
                                [ Toggles.checkbox Mdl
                                    [ 4 ]
                                    model.mdl
                                    [ Toggles.value True
                                    , Options.onToggle (ToggleClaim item.description)
                                    ]
                                    []
                                ]
                            ]
                        ]

                ClaimedBy name ->
                    Lists.ul []
                        [ Lists.li []
                            [ Lists.content [] [ text item.description ]
                            , Lists.content2 []
                                [ Toggles.checkbox Mdl
                                    [ 4 ]
                                    model.mdl
                                    [ Toggles.value False
                                    , Options.onToggle (ToggleClaim item.description)
                                    ]
                                    []
                                ]
                            , Lists.content2 [] [ text name ]
                            ]
                        ]
    in
        div []
            (List.map itemRow (Dict.values model.bill))
