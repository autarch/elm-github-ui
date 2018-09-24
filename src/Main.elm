module GitHubUI exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)

type alias Model =
    { message: String
    , repo: String
    }

init : flags -> ( Model, Cmd Msg )
init flags =
    ( { message = "Enter a GitHub repo by URL or name:"
      , repo = ""
      }, Cmd.none )

type Msg =
    LookUpRepo String

view : Model -> Browser.Document Msg
view model =
    Browser.Document "Elm GitHub UI" [body model]

body : Model -> Html Msg
body model =
    div []
        [ text model.message
        , render model
        ]

render : Model -> Html Msg
render model =
    if model.repo == "" then
        Html.form
            [ onSubmit (\v -> lookUpRepo model.repo) ]
            [ input
                  [ type_ "text", placeholder "GitHub repo", value model.repo, onSubmit (\v -> { model | repo = v }) ]
                  []
            , button
                  [ type_ "submit" ]
                  [ text "Submit" ]
            ]
    else
        text model.repo

lookUpRepo : String -> Msg
lookUpRepo repo =
    LookUpRepo repo

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LookUpRepo repo ->
            (
             { model
                 | message = "Looking up repo"
                 , repo = repo
             }
            , Cmd.none
            )

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
