module Basic exposing (main)


import Html exposing (Html, div, input, label, span, text)
import Html.App as Html
import Html.Lazy exposing (lazy)
import Html.Attributes exposing (id, checked, placeholder, type')
import Html.Events exposing (onCheck, onInput)
import String
import Result
import Task exposing (Task)
import SelectableText exposing (defaultOptions)


-- MODEL


type alias Model =
  { selectableTextModel : SelectableText.Model
  }


init : (Model, Cmd Msg)
init =
  let
    initialModel =
      Model 
        <| SelectableText.initialModel 
             { defaultOptions 
               | id = "my-text"
               , placeholderText = "Loading..."
               , allowInterparagraphSelection = True
               , maxSelectionLength = Nothing
               }
  in 
    initialModel ! [ getRawText ]


-- MESSAGES


type Msg
  = NoOp
  | SelectableTextMsg SelectableText.Msg
  | RawTextUnavailable String
  | RawTextReady String
  | AllowInterparagraphSelection Bool
  | MaxSelectionLength String


-- VIEW


view : Model -> Html Msg
view { selectableTextModel } =
  div [] 
    [ lazy viewOptions selectableTextModel
    , lazy viewSelectedPhrase selectableTextModel.selectedPhrase
    , Html.map SelectableTextMsg (SelectableText.view selectableTextModel)
    ]


viewOptions : SelectableText.Model -> Html Msg
viewOptions { options } =
  let
    maxSelectionLengthString =
      case options.maxSelectionLength of
        Just length ->
          toString length
        Nothing ->
          ""
  in
    div [ id "options" ]
      [ label []
          [ input 
              [ type' "checkbox"
              , checked options.allowInterparagraphSelection 
              , onCheck AllowInterparagraphSelection
              ]
              []
          , text "Allow selection to span across multiple paragraphs"
          ]
      , label []
          [ input 
              [ type' "text"
              , placeholder maxSelectionLengthString
              , onInput MaxSelectionLength
              ]
              []
          , text "Maximal selection length (number of words)"
          ]
      ]


viewSelectedPhrase : Maybe String -> Html Msg
viewSelectedPhrase maybeSelectedPhrase =
  div [ id "phrase-container" ] 
    [ text "Selected phrase: "
    , span [ ] [ text <| Maybe.withDefault "–" maybeSelectedPhrase ]
    ]


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ selectableTextModel } as model) =
  case msg of
    NoOp ->
      model ! []

    SelectableTextMsg subMsg ->
      let
        (newSelectableTextModel, selectableTextCmd) = 
          SelectableText.update subMsg selectableTextModel
      in
        { model | selectableTextModel = newSelectableTextModel } 
          ! [ Cmd.map SelectableTextMsg selectableTextCmd ]

    RawTextUnavailable _ ->
      model ! []

    RawTextReady rawText ->
      model 
        ! [ message
              <| SelectableTextMsg 
              <| SelectableText.RenderText rawText
          ] 

    -- don't do this at home, 
    -- TODO: Think about whether to provide a way to update the options
    AllowInterparagraphSelection bool ->
      let
        options =
          selectableTextModel.options
      in
        { model 
          | selectableTextModel = 
              { selectableTextModel 
                | options = 
                    { options 
                      | allowInterparagraphSelection = bool 
                      }
                } 
          }
          ! [ ]

    MaxSelectionLength inputtedLength ->
      let
        newMaxSelectionLength = 
          String.toInt inputtedLength 
            |> Result.toMaybe
        options =
          selectableTextModel.options
      in
        { model 
          | selectableTextModel = 
              { selectableTextModel 
                | options = 
                    { options 
                      | maxSelectionLength = newMaxSelectionLength
                      }
                } 
          }
          ! [ ]



message : Msg -> Cmd Msg
message msg =
  Task.perform identity identity (Task.succeed msg)


getRawText : Cmd Msg
getRawText =
  Task.perform RawTextUnavailable RawTextReady (Task.succeed sampleText)


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

sampleText : String
sampleText = """
Aujourd’hui, maman est morte. Ou peut-être hier, je ne sais pas. J’ai reçu un télégramme de l’asile: «Mère décédée. Enterrement demain. Sentiments distingués.» Cela ne veut rien dire. C’était peut-être hier.
L’asile de vieillards est à Marengo, à quatre-vingts kilomètres d’Alger. Je prendrai l’autobus à deux heures et j’arriverai dans l’après-midi. Ainsi, je pourrai veiller et je rentrerai demain soir. J’ai demandé deux jours de congé à mon patron et il ne pouvait pas me les refuser avec une excuse pareille. Mais il n’avait pas l’air content. Je lui ai même dit : «Ce n’est pas de ma faute.» Il n’a pas répondu. J’ai pensé alors que je n’aurais pas dû lui dire cela. En somme, je n’avais pas à m’excuser. C’était plutôt à lui de me présenter ses condoléances. Mais il le fera sans doute après-demain, quand il me verra en deuil. Pour le moment, c’est un peu comme si maman n’était pas morte. Après l’enterrement, au contraire, ce sera une affaire classée et tout aura revêtu une allure plus officielle.
J’ai pris l’autobus à deux heures. Il faisait très chaud. J’ai mangé au restaurant, chez Céleste, comme d’habitude. Ils avaient tous beaucoup de peine pour moi et Céleste m’a dit : «On n’a qu’une mère.» Quand je suis parti, ils m’ont accompagné à la porte. J’étais un peu étourdi parce qu’il a fallu que je monte chez Emmanuel pour lui emprunter une cravate noire et un brassard. Il a perdu son oncle, il y a quelques mois.
J’ai couru pour ne pas manquer le départ. Cette hâte, cette course, c’est à cause de tout cela sans doute, ajouté aux cahots, à l’odeur d’essence, à la réverbération de la route et du ciel, que je me suis assoupi. J’ai dormi pendant presque tout le trajet. Et quand je me suis réveillé, j’étais tassé contre un militaire qui m’a souri et qui m’a demandé si je venais de loin. J’ai dit «oui» pour n’avoir plus à parler.
"""


