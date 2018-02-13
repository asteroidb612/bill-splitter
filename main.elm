module Main exposing (main)

import Time
import Debug
import List
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (send, get, Error(..))
import Json.Encode as E
import Json.Decode as D
import Tuple exposing (first, second)
import Maybe exposing (Maybe(..))
import Material
import Material.Grid as Grid
import Material.Button as Button
import Material.Options as Options
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color
import Material.List as Lists
import Material.Toggles as Toggles
import Material.Textfield as Textfield
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Material.Chip as Chip

main =
    program
        { init = { name = "", owed = 0, bill = defaultDict, mdl = Material.model, claimedItems = [], message = "", named = False } ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        [ Layout.subs Mdl model.mdl
        , Time.every (300 * Time.millisecond) RefreshBill
        ]


items =
    [ ( "Bone Marrow", 17 )
    , ( "Blue Cheeseburger w Salad", 16 )
    , ( "Burger w Salad", 16 )
    , ( "Charcuterie platter", 16 )
    , ( "Chicken Wings, side of arugala salad", 12 )
    , ( "Fish and Chips", 18 )
    , ( "French Fries 1", 8 )
    , ( "French Fries 2", 8 )
    , ( "HH Abita Purple Haze", 5 )
    , ( "HH Craft Lager 1", 5 )
    , ( "HH Craft Lager 2", 5 )
    , ( "HH The Screws Are Loose 1", 5 )
    , ( "HH The Screws Are Loose 2", 5 )
    , ( "HH The Screws are Loose 3", 5 )
    , ( "HH The Screws are Loose 4", 5 )
    , ( "HH HefD ", 5 )
    , ( "HefD 16oz", 7.5 )
    , ( "Mac and Cheese", 15 )
    , ( "Open Kitchen Salad", 2 )
    , ( "Outer Space Canoe 16 oz", 8 )
    , ( "Spicy Burger w Fries", 17 )
    , ( "Stout as a Service", 6 )
    , ( "Tossed Salad", 10 )
    , ( "Waygu Burger with fries, medium", 20 )
    , ( "Waygu Burger with salad, white cheddar", 20 )
    ]


defaultDict =
    List.map (\x -> ( first x, (Item (first x) (second x) Unclaimed) )) items
        |> Dict.fromList


type alias Model =
    { name : String, owed : Float, bill : Dict String Item, mdl : Material.Model, claimedItems : List Item, message : String, named : Bool }


type Claim
    = Unclaimed
    | ClaimedBy String


type alias Item =
    { description : String, price : Float, claim : Claim }


type Msg
    = ToggleClaim String
    | NoOp
    | Mdl (Material.Msg Msg)
    | SaveBill (Result Http.Error (Dict.Dict String Item))
    | RefreshBill Time.Time
    | NameUpdate String
    | SaveName


handleError model e = model ! []
--    case e of
--        Http.Timeout ->
--            { model | message = "timeout" } ! []
--
--        Http.NetworkError ->
--            { model | message = "network error" } ! []
--
--        Http.BadUrl x ->
--            { model | message = "badurl:\n" ++ x } ! []
--
--        Http.BadStatus x ->
--            { model | message = "badstatus:\n" ++ (toString x) } ! []
--
--        Http.BadPayload x y ->
--            { model | message = "badpayload:\n" ++ x ++ "\n" ++ (toString y) } ! []


update msg model =
    let
        includeItem x =
            case x.claim of
                Unclaimed ->
                    False

                ClaimedBy name ->
                    name == model.name
    in
        case msg of
            ToggleClaim newDescription ->
                let
                    newBill =
                        Dict.update newDescription replace model.bill

                    newClaimedItems =
                        Dict.toList newBill
                            |> List.map second
                            |> List.filter includeItem

                    newSum =
                        newClaimedItems
                            |> List.map .price
                            |> List.sum

                    replace oldItem =
                        case oldItem of
                            -- TODO I think there's an Elm default to replace this
                            Nothing ->
                                Nothing

                            Just item ->
                                -- TODO This should probably be two helpers
                                Just
                                    { item
                                        | claim =
                                            case item.claim of
                                                Unclaimed ->
                                                    ClaimedBy model.name

                                                ClaimedBy _ ->
                                                    Unclaimed
                                    }

                    sendBill =
                        Http.send SaveBill
                in
                    { model
                        | bill = newBill
                        , owed = newSum
                        , claimedItems = newClaimedItems
                    }
                        ! [ sendBill <| putRequest newBill ]

            SaveBill (Ok remoteBill) ->
                { model | bill = remoteBill } ! []

            SaveBill (Err e) ->
                handleError model e

            RefreshBill time ->
                model ! [ Http.send SaveBill <| Http.get urlBase decodeBill ]

            NameUpdate name ->
                { model | name = name } ! []

            SaveName ->
                { model | named = True } ! []

            _ ->
                model ! []


ownerOf item =
    case item.claim of
        Unclaimed ->
            ""

        ClaimedBy name ->
            name


encodeItem item =
    E.object
        [ ( "description", E.string item.description )
        , ( "owner", E.string <| ownerOf item )
        , ( "price", E.float item.price )
        ]


encodeBill bill =
    Dict.toList bill
        |> List.map (\( string, item ) -> ( string, encodeItem item ))
        |> E.object


discernClaim jsonClaim =
    case jsonClaim of
        "" ->
            Unclaimed

        name ->
            ClaimedBy name


decodeClaim =
    D.map discernClaim D.string


decodeItem =
    D.map3 Item (D.field "description" D.string) (D.field "price" D.float) (D.field "owner" decodeClaim)


decodeBill =
    D.dict decodeItem


urlBase =
    "https://pebble-timetracking.firebaseio.com/bill.json"


putRequest bill =
    Http.request
        { method = "PUT"
        , headers = []
        , url = urlBase
        , body = Http.jsonBody (encodeBill bill)
        , expect = Http.expectJson decodeBill
        , timeout = Nothing
        , withCredentials = False
        }


view : Model -> Html Msg
view model =
    let
        chip item =
            Chip.span
                [ Options.css "margin" "10px 5px"
                , Chip.deleteIcon "cancel"
                , Chip.deleteClick (ToggleClaim item.description)
                ]
                [ Chip.content []
                    [ text (" $" ++ toString item.price ++ " " ++ item.description) ]
                ]

        common =
            [ ( "vertical-align", "middle" ), ( "text-align", "center" ), ( "display", "inline-block" ) ]

        spot x =
            span [ style <| ( "margin", "0 .8em" ) :: common ] [ text x ]

        num x =
            span [ style <| ( "font-size", "2em" ) :: common ] [ text x ]

        topText =
            if List.isEmpty model.claimedItems then
                div [] [h3 [] [ text "Mary's Birthday Dinner at the Halford" ]
                       , if model.named
                         then h4 [] [ text "What did you have?"]
                         else h4 [] [ ]
                       ]
            else
                div []
                    [ text model.message
                    , num <| "$" ++ format usLocale model.owed
                    , spot <| "+ 9% Sales Tax = "
                    , num <| "$" ++ format usLocale (model.owed * 1.09)
                    , spot <| "+ 18% Gratuity ="
                    ,  Button.render Mdl
                        [ 9, 0, 0, 1 ]
                        model.mdl
                        [ Button.ripple
                        , Button.colored
                        , Button.raised
                        , Button.link  <|
                               "https://venmo.com/?txn=pay&note=Mary%27s%20Dinner&amount=" ++ format usLocale (model.owed * 1.18 * 1.09)
                        ]
                        [ text <| "Venmo $" ++ format usLocale (model.owed * 1.18 * 1.09)  ]
                    ]
    in
        Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
            Layout.render Mdl
                model.mdl
                [ Layout.fixedHeader
                ]
                { header =
                    [ div [ style [ ( "padding", "1rem" ) ] ] <|
                        topText
                            :: List.map chip model.claimedItems
                    ]
                , drawer = []
                , tabs = ( [], [] )
                , main = mainView model
                }


mainView model =
    let
        name item =
            case item of
                ClaimedBy name ->
                    Just name

                Unclaimed ->
                    Nothing

        names =
            Dict.toList model.bill
                |> List.filterMap (Tuple.second >> .claim >> name)
                |> Debug.log "Names: "

        repeatedName =
            List.member (Debug.log "Name : " model.name) names

        namePick =
            Textfield.render Mdl
                [ 4 ]
                model.mdl
                [ Options.onInput NameUpdate
                , Textfield.error ("Name Taken")
                    |> Options.when repeatedName
                ]
                []
    in
        if model.named then
            [ viewBody model ]
        else [Lists.ul []
                     [Lists.li [] [namePick]
                     , Lists.li []
                         [ Button.render Mdl
                             [ 9, 0, 0, 1 ]
                                 model.mdl
                                     [ Button.ripple
                                     , Button.colored
                                     , Button.raised
                                     , if repeatedName || model.name == "" then
                                           Button.disabled
                                       else
                                           Options.nop
                                     , Options.onClick SaveName
                                     ]
                                 [ text <| if model.name == "" then "Enter your name " else  "Pick what you ordered" ]
                         ]
                     ]
             ]
            
viewBody : Model -> Html Msg
viewBody model =
    let
        itemRow item =
            case item.claim of
                Unclaimed ->
                    Lists.li []
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
                        ]

                ClaimedBy name ->
                    if name == model.name then
                        Lists.li []
                            [ Lists.content [Options.css "text-decoration" "underline"] [ text item.description ]
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
                    else
                        Lists.li [] [ Lists.content [Options.css "text-decoration" "line-through"] [ text item.description ] ]
    in
        (List.map itemRow (Dict.values model.bill))
            |> Lists.ul []
            |> \x ->
                [ x ]
                    |> div []
