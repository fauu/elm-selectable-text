module SelectableText exposing 
  (Model
  , Msg (ParseText)
  , initialModel
  , view
  , update
  )

{-| A selectable text component that renders provided text as a series
of paragraphs and `span` tags containing single words and punctuation. It lets 
the user select words using mouse. The model contains the selected phrase in 
form of a string as well as the previously selected one. That provides an easy 
way of detection of a selection change within the parent component.

# Model
@docs Model, initialModel

# Messages
@docs Msg

# View function
@docs view

# Update function
@docs update
-}

import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)
import String
import List
import List.Extra
import Regex exposing (HowMany (All), find, regex)
import Dict exposing (Dict)


-- MODEL


{-| The component model:
  - **id** - Used as the HTML id attribute for the root tag
-}
type alias Model =
  { id : String
  , text : Text
  , mouseOverWordNo : Maybe Int
  , selecting : Bool
  , selection : Maybe Selection
  , selectedPhrase : Maybe String
  , previousSelectedPhrase : Maybe String
  }


type TextElement
  = Word String
  | Punctuation String
  | ParagraphBreak


type alias SelectionIndicator = 
  Bool


type alias TextElementWithSelectionIndicator =
  (TextElement, SelectionIndicator)


type alias ElementNo =
  Int


type alias TextElementWithMetadata =
  (ElementNo, TextElementWithSelectionIndicator)


type alias Paragraph =
  List TextElementWithMetadata


type alias Text =
  Dict ElementNo TextElementWithSelectionIndicator


{-| A type representing a selection as ids of three text elements:
  - The initial element of the selection
  - First element of the selection
  - Last element of the selection
-}
type alias Selection =
  (ElementNo, ElementNo, ElementNo)


{-| Initialize the component. Expects user to provide id and a placeholder text
that will be displayed until a text is provided by the parent using `ParseText`
message.


    selectableTextModel = SelectableText.initialModel "my-text" "Loading..."
  
-}
initialModel : String -> String -> Model
initialModel id' placeholderText =
  { id = id'
  , text = Dict.singleton 0 (Word placeholderText, False)
  , mouseOverWordNo = Nothing
  , selecting = False
  , selection = Nothing
  , selectedPhrase = Nothing
  , previousSelectedPhrase = Nothing
  }


-- MESSAGES


{-| A type representing component messages. The `ParseText` message should be
used to pass a string to be parsed and rendered by the component from the parent
component.


    RawTextFetched rawText ->
      model 
        ! [ message
              <| SelectableTextMsg 
              <| SelectableText.ParseText rawText
          ] 

-}
type Msg
  = NoOp
  | StartSelecting
  | StopSelecting
  | MouseEnteredWord ElementNo
  | MouseLeftWord ElementNo
  | ParseText String


-- VIEW


{-| The selectable text view. Renders a `div` element containing the text.
-}
view : Model -> Html Msg
view model = 
  let
    paragraphs =
      List.map viewParagraph (Dict.toList model.text |> splitIntoParagraphs)
  in
    div 
      [ id model.id 
      , onMouseDown StartSelecting
      , onMouseUp StopSelecting 
      ] 
      paragraphs


splitIntoParagraphs : List TextElementWithMetadata -> List Paragraph
splitIntoParagraphs textElements =
  let
    isNotParagraphBreak (_, (textElement, _)) =
      case textElement of
        ParagraphBreak ->
          False
        _ ->
          True
  in
    (::) (List.Extra.takeWhile isNotParagraphBreak textElements)
      <| 
        let
          maybeTail =
            List.tail 
              <| List.Extra.dropWhile isNotParagraphBreak textElements
        in
          case maybeTail of
            Just tail ->
              splitIntoParagraphs tail
            Nothing ->
              []


viewParagraph : Paragraph -> Html Msg
viewParagraph paragraph =
  p [] (List.map viewTextElement paragraph)


viewTextElement : TextElementWithMetadata -> Html Msg
viewTextElement (no, (textElement, isSelected)) =
  case textElement of
    Word w ->
      let
        class' = 
          "w-" 
            ++ (toString no) 
            ++ if isSelected 
                 then " selected" 
                 else ""
      in
        span 
          [ class class'
          , onMouseEnter (MouseEnteredWord no)
          , onMouseLeave (MouseLeftWord no)
          ] 
          [ text w ]
    Punctuation p ->
      if isSelected then 
        span [ class "selected" ] [ text p ]
      else 
        text p
    ParagraphBreak ->
      text ""


-- UPDATE


{-| The selectable text update function.
-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []

    StartSelecting ->
      let
        newSelection =
          case model.mouseOverWordNo of
            Just no ->
              Just (no, no, no)
            Nothing ->
              Nothing
        newText =
          markSelection newSelection model.text
        newModel =
          { model
            | selecting = True
            , selection = newSelection
            , text = newText
            }
      in
        newModel ! []

    StopSelecting ->
      let
        newSelectedPhrase =
          selectedPhrase model.selection model.text
      in
        { model 
          | selecting = False
          , selectedPhrase = newSelectedPhrase
          , previousSelectedPhrase = model.selectedPhrase
          } 
          ! [ ]

    MouseEnteredWord no ->
      let
        newSelection =
          if model.selecting then
              Just <| recalculateSelection no model.selection
          else
              model.selection
        newText = 
          if model.selecting then
            markSelection newSelection model.text
          else
            model.text
        newModel =
          { model
            | mouseOverWordNo = (Just no)
            , selection = newSelection
            , text = newText
            } 
      in
        newModel ! []

    MouseLeftWord no ->
      { model | mouseOverWordNo = Nothing } ! []

    ParseText rawText ->
      { model | text = parseRawText rawText } ! []


{-| Recalculates the selection given a new element number 
-}
recalculateSelection : ElementNo -> Maybe Selection -> Selection
recalculateSelection newNo maybeSelection =
  case maybeSelection of
    Just (initialNo, _, _) ->
      (initialNo, min initialNo newNo, max initialNo newNo)
    Nothing ->
      (newNo, newNo, newNo)


{-| Given a selection, sets the selection indicator for the appropriate text
elements.
-}
markSelection : Maybe Selection -> Text -> Text
markSelection maybeSelection text =
  let
    newIsSelected no =
      case maybeSelection of
        Just (_, start, end) ->
          no >= start && no <= end
        Nothing ->
          False
  in
    Dict.map 
      (\no (textElement, isSelected) -> (textElement, newIsSelected no)) 
      text


{-| Given a selection and a text, returns the selected phrase as a string
-}
selectedPhrase : Maybe Selection -> Text -> Maybe String
selectedPhrase maybeSelection text =
  let
    textElementText textElement =
      case textElement of
        Word w ->
          w
        Punctuation p ->
          p
        _ ->
          ""
  in
    case maybeSelection of
      Just (_, start, end) ->
        Dict.filter (\no _ -> no >= start && no <= end) text 
          |> Dict.toList
          |> List.map (\(_, (textElement, _)) -> textElementText textElement)
          |> String.concat
          |> Just
      Nothing ->
        Nothing


parseRawText : String -> Text
parseRawText rawText =
  find All (regex "([^.,;\"?!\\s]+)|([.,;\"?! \\t]+)|([\\r\\n]+)") rawText
    |> List.map parseRawTextElement
    |> Dict.fromList


parseRawTextElement : Regex.Match -> TextElementWithMetadata
parseRawTextElement match =
  let
    textElement =
      case match.submatches of
        [Just word, _, _] ->
          Word word 
        [_, Just punctuaction, _] ->
          Punctuation punctuaction
        [_, _, Just paragraphBreak] ->
          ParagraphBreak
        _ ->
          Punctuation " "
  in
    (match.number, (textElement, False))
