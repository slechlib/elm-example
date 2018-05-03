module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Navigation exposing (Location)
import UrlParser exposing ((</>))

-- Entrypoint (we use Navigation)
main : Program Never Model Msg
main = Navigation.program UrlChange
       { view = view
       , update = update
       , subscriptions = (\_ -> Sub.none)
       , init = init
       }

-- Model = state of the app
type alias Model =
    { page : Page
    , metres : Float
    , token : GitHubToken
    , githubData : String
    }

-- Page = enum of different views
type Page
    = Home
    | UnitConverter
    | GitHubInfo
    | NotFound

-- Own type for GitHub token
type GitHubToken
    = Valid String
    | Invalid String

-- Units for the conversion
type LengthUnit
    = Metres
    | Inches
    | Yards
    | Feets

-- Types of messages in the app with content type(s)
type Msg
    = UrlChange Location
    | UnitUpdate LengthUnit String
    | TokenUpdate String
    | GitHubResponse (Result Http.Error String)

-- Initial app state and command
init : Location -> ( Model, Cmd Msg )
init location = urlUpdate location { page = Home
                                   , metres = 0
                                   , token = Invalid ""
                                   , githubData = ""
                                   }

-- Update (when message comes, update model), this is just "router"
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      UrlChange location ->
          urlUpdate location model
      UnitUpdate lu str ->
          metresUpdate lu str model
      TokenUpdate str ->
          tokenUpdate str model
      GitHubResponse res ->
          githubUpdate res model

githubUpdate : (Result Http.Error String) -> Model -> (Model, Cmd Msg)
githubUpdate res model =
    case res of
        Ok str -> ({ model | githubData = str }, Cmd.none)
        Err _  -> ({ model | githubData = "Error!" }, Cmd.none)

tokenUpdate : String -> Model -> (Model, Cmd Msg)
tokenUpdate str model =
    if isTokenValid str then
        ( { model | token = Valid str }, gitHubInfoRequest str )
    else
        ( { model | token = Invalid str }, Cmd.none )

-- Send request to GitHub and then it will send appropriate message in this app
gitHubInfoRequest : String -> Cmd Msg
gitHubInfoRequest token =
    Http.send GitHubResponse <| Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("token " ++ token)]
        , url = "https://api.github.com/user"
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

isTokenValid : String -> Bool
isTokenValid str = String.length str == 40

metresUpdate : LengthUnit -> String -> Model -> ( Model, Cmd Msg )
metresUpdate lu x model =
    case String.toFloat x of
        Ok v -> ( { model | metres = v / (unitCoefficient lu)}, Cmd.none )
        Err _ -> ( model, Cmd.none )

urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )
        Just route ->
            ( { model | page = route }, Cmd.none )

decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location

routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map UnitConverter (UrlParser.s "unit-converter")
        , UrlParser.map GitHubInfo (UrlParser.s "github-info")
        ]

--> VIEW
view : Model -> Html Msg
view model =
    div []
        [ menu model
        , mainContent model
        ]

menu : Model -> Html Msg
menu model =
    div []
        [ viewLink "" "Home"
        , viewLink "unit-converter" "Unit Converter"
        , viewLink "github-info" "GitHub Info"
        ]

viewLink : String -> String -> Html msg
viewLink slug name =
  li [] [ a [ href ("#" ++ slug) ] [ text name ] ]

mainContent : Model -> Html Msg
mainContent model =
    div [] (
        case model.page of
            Home ->
                pageHome model
            UnitConverter ->
                pageUnitConverter model
            GitHubInfo ->
                pageGitHubInfo model
            NotFound ->
                pageNotFound
    )

pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Home" ]
    , p [] [ text "This is very simple Elm example" ]
    , hr [] []
    , p [] [ text "Enjoy learning "
           , a [href "http://elm-lang.org"] [text "Elm"]
           , text "!"
           ]
    ]

pageUnitConverter : Model -> List (Html Msg)
pageUnitConverter model =
    [ h1 [] [ text "Unit Converter" ]
    , hr [] []
    , makeUnitInput Metres model
    , makeUnitInput Inches model
    , makeUnitInput Feets model
    , makeUnitInput Yards model
    ]

makeUnitInput : LengthUnit -> Model -> Html Msg
makeUnitInput lu model =
    div []
        [ label [] [text (unitToString lu)]
        , input [ type_ "number"
                , onInput (UnitUpdate lu)
                , value (toString (computeUnit lu model))
                ]
                []
        ]

pageGitHubInfo : Model -> List (Html Msg)
pageGitHubInfo model =
    [ h1 [] [ text "GitHub Info" ]
    , div []
          [ label [] [text "GitHub token: "]
          , input [ type_ "text"
                  , onInput TokenUpdate
                  , value (tokenToString model.token)
                  ]
                  []
          ]
    , case model.token of
          Valid token -> githubInfo model
          Invalid _ -> invalidTokenMsg
    ]

githubInfo : Model -> (Html Msg)
githubInfo model =
    pre [] [text (model.githubData)]


invalidTokenMsg : (Html Msg)
invalidTokenMsg =
    div []
        [ p [] [text "Your token is not valid (40 chars required)"]
        ]

tokenToString : GitHubToken -> String
tokenToString t =
    case t of
        Valid s -> s
        Invalid s -> s

pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]

--> LOGIC
unitToString : LengthUnit -> String
unitToString lu =
    case lu of
        Metres -> "Metres"
        Inches -> "Inches"
        Yards -> "Yards"
        Feets -> "Feets"

computeUnit : LengthUnit -> Model -> Float
computeUnit lu model = model.metres * (unitCoefficient lu)

unitCoefficient : LengthUnit -> Float
unitCoefficient lu =
    case lu of
        Metres -> 1
        Inches -> 39.3700787
        Yards -> 1.0936133
        Feets -> 3.2808399
