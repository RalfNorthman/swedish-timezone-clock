module Main exposing (..)

import Browser
import Time exposing (Posix, Zone)
import Html exposing (Html)
import Element exposing (..)
import TimeZone exposing (getZone)
import Task exposing (attempt)
import DateFormat as Format
import DateFormat.Language exposing (swedish)


---- MODEL ----


type alias Model =
    { zoneName : String
    , zone : Zone
    , time : Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { zoneName = "Nothing yet."
      , zone = Time.utc
      , time = Time.millisToPosix 0
      }
    , attempt GetZone getZone
    )



---- UPDATE ----


type Msg
    = GetZone (Result TimeZone.Error ( String, Zone ))
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetZone (Ok ( name, zone )) ->
            ( { model | zoneName = name, zone = zone }, Cmd.none )

        GetZone (Err error) ->
            ( { model | zoneName = "Attempt failed." }, Cmd.none )

        Tick posix ->
            ( { model | time = posix }, Cmd.none )



---- VIEW ----


myFormat : List Format.Token -> Model -> String
myFormat tokens model =
    Format.formatWithLanguage
        swedish
        tokens
        model.zone
        model.time


clockFormat : Model -> String
clockFormat model =
    myFormat
        [ Format.hourMilitaryFixed
        , Format.text ":"
        , Format.minuteFixed
        , Format.text ":"
        , Format.secondFixed
        ]
        model


dateFormat : Model -> String
dateFormat model =
    myFormat
        [ Format.dayOfMonthSuffix
        , Format.text " "
        , Format.monthNameFull
        ]
        model


myText : String -> Element msg
myText string =
    el [ centerX ] <| text string


view : Model -> Html Msg
view model =
    layout [] <|
        column
            [ padding 20
            , spacing 8
            ]
            [ myText model.zoneName
            , myText <| clockFormat model
            , myText <| myFormat [ Format.dayOfWeekNameFull ] model
            , myText <| dateFormat model
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
