module GitHubUI exposing (..)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode exposing (bool, int, string, nullable, succeed, Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import String

type alias User =
    { login: String
    , avatarURL: String
    , htmlURL: String
    , type_: String
    , reposURL: String
    }

type alias License =
    { key: String
    , name: String
    }

type LinkedRepo = Maybe Repo

type alias Repo =
    { fullName: String
    , owner: User
    , htmlURL: String
    , description: String
    , contributorsURL: String
    , fork: Bool
    , forksCount: Int
    , watchersCount: Int
    , openIssuesCount: Int
    , license: License
    , parent: LinkedRepo
    , source: LinkedRepo
    }

type alias Model =
    { message: String
    , repoName: String
    , response: Maybe Http.Response
    , repo: Maybe Repo
    , user: Maybe User
    }

init : flags -> ( Model, Cmd Msg )
init flags =
    ( { message = "Enter a GitHub repo by URL or name:"
      , repoName = ""
      , response = Nothing
      , repo = Nothing
      , user = Nothing
      }, Cmd.none )

type Msg
    = NoOp
    | LookUpRepo String
    | SetRepoName String
    | GetRepo (Result Http.Error String)

view : Model -> Browser.Document Msg
view model =
    Browser.Document "Elm GitHub UI" [ body model ]

body : Model -> Html Msg
body model =
    div []
        [ text model.message
        , render model
        ]

render : Model -> Html Msg
render model =
    div []
        [ Html.form
              [ onSubmit (LookUpRepo model.repoName) ]
              [ input
                    [ type_ "text"
                    , placeholder "GitHub repo"
                    , value model.repoName
                    , onInput SetRepoName ]
                    []
              , button
                    [ type_ "submit" ]
                    [ text "Submit" ]
              ]
        , currentState model
        ]

currentState : Model -> Html Msg
currentState model =
    if model.repoName == "" then
        text "No repo provided yet."
    else
        text ("Will search for " ++ model.repoName)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        SetRepoName repo ->
            ( { model | repoName = repo }, Cmd.none )
        LookUpRepo repo ->
            ( { model | message = "Looking up repo", response = repoHttpRequest model.repoName, repo = Nothing, user = Nothing }, Cmd.none )
        GetRepo (Ok response) ->
            ( { model | response = Just response }, Cmd.none )
        GetRepo (Err error) ->
            ( { model | response = Just (httpErrorString error) }, Cmd.none )

repoHttpRequest : String -> Cmd Msg
repoHttpRequest name =
    Http.send GetRepo <|
        Http.get ("https://api.github.com/repos/" ++ name) repoDecoder

repoDecoder : Decoder
repoDecoder =
    succeed Repo
        |> required "full_name" string
        |> required "owner" userDecoder
        |> required "html_url" string
        |> required "description" (nullable string)
        |> required "contributors_url" string
        |> required "fork" bool
        |> required "forks_count" int
        |> required "watchers_count" int
        |> required "open_issues_count" int
        |> required "license" licenseDecoder
        |> optional "parent" repoDecoder
        |> optional "source" repoDecoder

userDecoder: Decoder
userDecoder =
    succeed User
        |> required "login" string
        |> required "avatar_url" string
        |> required "html_url" string
        |> required "type" string
        |> required "repos_url" string

licenseDecoder: Decoder
licenseDecoder =
    succeed License
        |> required "key" string
        |> required "name" string

httpErrorString: Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl msg ->
            msg
        Http.Timeout ->
            "request timed out"
        Http.NetworkError ->
            "network error"
        Http.BadStatus response ->
            "request returned a " ++ String.fromInt response.status.code ++ " response"
        Http.BadPayload payload response ->
            "response had a bad payload in the body: " ++ payload ++ " - error: " ++ response.body

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program () Model Msg
main =
    Browser.document
        { init = init
          , view = view
          , update = update
          , subscriptions = subscriptions
        }
