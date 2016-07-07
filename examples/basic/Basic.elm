module Basic exposing (main)


import Html exposing (Html, div, p, span, text)
import Html.App as Html
import Html.Attributes exposing (id)
import Html.Lazy exposing (lazy)
import Task exposing (Task)
import SelectableText


-- MODEL


type alias Model =
  { selectableTextModel : SelectableText.Model
  }


init : (Model, Cmd Msg)
init =
  let
    initialModel = Model <| SelectableText.initialModel "my-text" "Loading..."
  in 
    initialModel ! [ getRawText ]


-- MESSAGES


type Msg
  = NoOp
  | SelectableTextMsg SelectableText.Msg
  | RawTextUnavailable String
  | RawTextReady String


-- VIEW


view : Model -> Html Msg
view model =
  div 
    [] 
    [ lazy viewSelectedPhrase model.selectableTextModel.selectedPhrase
    , Html.map SelectableTextMsg (SelectableText.view model.selectableTextModel)
    ]


viewSelectedPhrase : Maybe String -> Html Msg
viewSelectedPhrase maybeSelectedPhrase =
  p 
    [] 
    [ text "Selected phrase: "
    , span [ id "phrase" ] [ text <| Maybe.withDefault "–" maybeSelectedPhrase ]
    ]


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []

    SelectableTextMsg subMsg ->
      let
        (newSelectableTextModel, selectableTextCmd) = 
          SelectableText.update subMsg model.selectableTextModel
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
Aujourd’hui, maman est morte. Ou peut-être hier, je ne sais pas. J’ai reçu un télégramme de l’asile : « Mère décédée. Enterrement demain. Sentiments distingués. » Cela ne veut rien dire. C’était peut-être hier.
L’asile de vieillards est à Marengo, à quatre-vingts kilomètres d’Alger. Je prendrai l’autobus à deux heures et j’arriverai dans l’après-midi. Ainsi, je pourrai veiller et je rentrerai demain soir. J’ai demandé deux jours de congé à mon patron et il ne pouvait pas me les refuser avec une excuse pareille. Mais il n’avait pas l’air content. Je lui ai même dit : « Ce n’est pas de ma faute. » Il n’a pas répondu. J’ai pensé alors que je n’aurais pas dû lui dire cela. En somme, je n’avais pas à m’excuser. C’était plutôt à lui de me présenter ses condoléances. Mais il le fera sans doute après-demain, quand il me verra en deuil. Pour le moment, c’est un peu comme si maman n’était pas morte. Après l’enterrement, au contraire, ce sera une affaire classée et tout aura revêtu une allure plus officielle.
J’ai pris l’autobus à deux heures. Il faisait très chaud. J’ai mangé au restaurant, chez Céleste, comme d’habitude. Ils avaient tous beaucoup de peine pour moi et Céleste m’a dit : « On n’a qu’une mère. » Quand je suis parti, ils m’ont accompagné à la porte. J’étais un peu étourdi parce qu’il a fallu que je monte chez Emmanuel pour lui emprunter une cravate noire et un brassard. Il a perdu son oncle, il y a quelques mois.
J’ai couru pour ne pas manquer le départ. Cette hâte, cette course, c’est à cause de tout cela sans doute, ajouté aux cahots, à l’odeur d’essence, à la réverbération de la route et du ciel, que je me suis assoupi. J’ai dormi pendant presque tout le trajet. Et quand je me suis réveillé, j’étais tassé contre un militaire qui m’a souri et qui m’a demandé si je venais de loin. J’ai dit « oui » pour n’avoir plus à parler.
"""


