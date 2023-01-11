module Main exposing (..)

import Browser
import Task
import Time
import Json.Decode as Decode exposing (Decoder, field, list, string, float)
import Http
import Html exposing (Html, table, thead, tbody, tr, td, text)
import String exposing (fromFloat, toFloat)

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { exchange_rates : List (String, Float)
    }

  
init : ( Model, Cmd Msg )
init =
    ( { exchange_rates = [] }, Http.get
              { url = "https://lcd.kaiyo.kujira.setten.io/oracle/denoms/exchange_rates"
              , expect = Http.expectJson 
                                (\response1 ->
                                    let
                                        _ =
                                            Debug.log "Exchange: Rates " response1
                                    in
                                    RatesDecoded response1
                                )
                                 exchangeRate
                                 } )




-- Define the `FetchExchangeRates` message
type Msg = RequestRates 
          |RatesDecoded (Result Http.Error (List(String, Float)))

-- Define a decoder for the JSON data returned by the URL
exchangeRate : Decode.Decoder (List (String, Float))
exchangeRate = 
    Decode.field "exchange_rates" 
        (Decode.list 
            (Decode.map2 
                (\denom amount -> (denom, amount))
                (field "denom" string)
                (Decode.field "amount" string
                    |> Decode.andThen 
                        (\stringValue ->
                            case String.toFloat stringValue of
                                Just floatValue -> 
                                    Decode.succeed (floatValue)
                                Nothing -> 
                                    Decode.fail "amount field is not parseable as float"
                        )
                )
            )
        )





update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    
        RequestRates ->
            (model,
            Http.get
              { url = "https://lcd.kaiyo.kujira.setten.io/oracle/denoms/exchange_rates"
              , expect = Http.expectJson 
                                (\response1 ->
                                    let
                                        _ =
                                            Debug.log "Exchange: Rates " response1
                                    in
                                    RatesDecoded response1
                                )
                                 exchangeRate
                                 })
        
        RatesDecoded (Ok exchange_rates) ->
            ( Model exchange_rates
            , Cmd.none
            )

        RatesDecoded (Err error) ->
            ( model, Cmd.none )




view : Model -> Html Msg
view model =
    table []
        [ thead []
            [ tr []
                [ td [] [ text "Denom" ]
                , td [] [ text "Exchange Rate" ]
                ]
            ]
        , tbody []
            (List.map (\(denom, amount) ->
                tr []
                    [ td [] [ text denom ]
                    , td [] [ text (String.fromFloat amount) ]
                    ]
            ) model.exchange_rates)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 30000 (\_ -> RequestRates) ]



