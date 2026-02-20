port module PhotoGroove exposing (..)

{-| ==========================================================
   PHOTO GROOVE - FULLY COMMENTED VERSION
   ----------------------------------------------------------
   This is a detailed educational version of the file.
   Every important section is explained with comments.
   ==========================================================
-}

-- IMPORTS ---------------------------------------------------

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, name, src, title, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


-- CONSTANTS -------------------------------------------------

-- Base URL for loading images
urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


-- MESSAGES --------------------------------------------------

{-| Msg represents every possible event in the app.
    In Elm, NOTHING happens unless it is represented
    as a message handled in `update`.
-}

type Msg
    = ClickedPhoto String              -- User clicked a thumbnail
    | ClickedSize ThumbnailSize        -- User selected thumbnail size
    | ClickedSurpriseMe                -- User clicked random button
    | GotRandomPhoto Photo             -- Random generator produced a photo
    | GotActivity String               -- JS sent activity info through port
    | GotPhotos (Result Http.Error (List Photo)) -- HTTP response received
    | SlidHue Int                      -- Hue slider changed
    | SlidRipple Int                   -- Ripple slider changed
    | SlidNoise Int                    -- Noise slider changed


-- VIEW ------------------------------------------------------

{-| Main view function.
    Renders different UI depending on current status.
-}
view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


-- FILTER SLIDER VIEW ---------------------------------------

{-| Creates a single slider (Hue / Ripple / Noise).
    toMsg converts slider value into a message.
-}
viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"                          -- Maximum slider value
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg                           -- Custom slide event
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


-- LOADED VIEW ----------------------------------------------

{-| Renders full UI when photos are loaded.
-}
viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick ClickedSurpriseMe ] [ text "Random selection" ]
    , div [ class "activity" ] [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas [ id "main-canvas", class "large" ] []
    ]


-- THUMBNAIL VIEW -------------------------------------------

{-| Renders one thumbnail image.
    Adds "selected" class if currently chosen.
-}
viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


-- SIZE SELECTOR --------------------------------------------

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


-- PORTS -----------------------------------------------------

{-| Sends filter options to JavaScript.
    JS will apply filters to canvas.
-}
port setFilters : FilterOptions -> Cmd msg


{-| Receives activity changes from JavaScript.
-}
port activityChanges : (String -> msg) -> Sub msg


-- DATA TYPES ------------------------------------------------

type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


-- JSON DECODER ----------------------------------------------

{-| Converts JSON into Photo type safely.
-}
photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


-- STATUS ----------------------------------------------------

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


-- MODEL -----------------------------------------------------

type alias Model =
    { status : Status
    , activity : String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , activity = ""
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }


type ThumbnailSize
    = Small
    | Medium
    | Large


-- UPDATE ----------------------------------------------------

{-| Central state machine of the app.
    Every message is handled here.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                _ ->
                    ( model, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    applyFilters
                        { model
                            | status = Loaded photos first.url
                        }

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }


-- APPLY FILTERS ---------------------------------------------

applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        _ ->
            ( model, Cmd.none )


-- HELPERS ---------------------------------------------------

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        _ ->
            status


-- INITIAL COMMAND -------------------------------------------

initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "https://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }


-- MAIN ------------------------------------------------------

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- SUBSCRIPTIONS ---------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity


-- CUSTOM RANGE SLIDER NODE ---------------------------------

rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


-- CUSTOM SLIDE EVENT DECODER --------------------------------

onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"