module SelectableText exposing 
  ( Model
  , Msg (RenderText)
  , initialModel
  , view
  , update
  )

{-| A selectable text component that renders provided text and lets the 
user select words using mouse. All the elements of the selection are assigned a 
`selected` css class. The model contains the selected phrase in form of a string
to be used by the parent component.

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
import Html.Attributes exposing (attribute, class, id)
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
  , mouseOverWordNo : Maybe ElementNo
  , selecting : Bool
  , selection : Maybe Selection
  , selectedPhrase : Maybe String
  }


type Element
  = Word String
  | Punctuation String
  | ParagraphBreak


type alias SelectionIndicator = 
  Bool


type alias ElementWithMetadata =
  (Element, SelectionIndicator)


type alias ElementNo =
  Int


type alias NumberedElementWithMetadata =
  (ElementNo, ElementWithMetadata)


type alias Paragraph =
  List NumberedElementWithMetadata


type alias Text =
  Dict ElementNo ElementWithMetadata


{-| A type representing a selection as ids of three text elements:
  - The initial element of the selection
  - First element of the selection
  - Last element of the selection
-}
type alias Selection =
  (ElementNo, ElementNo, ElementNo)


{-| Initialize the component. Expects user to provide id and a placeholder text
that will be displayed until a text is provided by the parent using `RenderText`
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
  }


-- MESSAGES


{-| A type representing component messages. The `RenderText` message should be
used to pass a string to be parsed and rendered by the component from the parent
component.


    message : Msg -> Cmd Msg
    message msg =
      Task.perform identity identity (Task.succeed msg)

    -- inside parent's update function
    RawTextFetched rawText ->
      model 
        ! [ message
              <| SelectableTextMsg 
              <| SelectableText.RenderText rawText
          ] 

-}
type Msg
  = NoOp
  | StartSelecting
  | StopSelecting
  | MouseEnteredWord ElementNo
  | MouseLeftWord ElementNo
  | RenderText String


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


splitIntoParagraphs : List NumberedElementWithMetadata -> List Paragraph
splitIntoParagraphs elements =
  let
    isNotParagraphBreak (_, (element, _)) =
      case element of
        ParagraphBreak ->
          False
        _ ->
          True
  in
    (::) (List.Extra.takeWhile isNotParagraphBreak elements)
      <| 
        let
          maybeTail =
            List.tail 
              <| List.Extra.dropWhile isNotParagraphBreak elements
        in
          case maybeTail of
            Just tail ->
              splitIntoParagraphs tail
            Nothing ->
              []


viewParagraph : Paragraph -> Html Msg
viewParagraph paragraph =
  p [] (List.map viewElement paragraph)


viewElement : NumberedElementWithMetadata -> Html Msg
viewElement (no, (element, isSelected)) =
  case element of
    Word w ->
      let
        classNode =
          if isSelected
            then [ class "selected" ]
            else [ ]
        attributes =
          [ onMouseEnter (MouseEnteredWord no)
          , onMouseLeave (MouseLeftWord no)
          ] 
          ++ classNode
      in
        span attributes [ text w ]
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
      { model 
        | selecting = False
        , selectedPhrase = selectedPhrase model.selection model.text
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

    RenderText rawText ->
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
      (\no (element, isSelected) -> (element, newIsSelected no)) 
      text


{-| Given a selection and a text, returns the selected phrase as a string
-}
selectedPhrase : Maybe Selection -> Text -> Maybe String
selectedPhrase maybeSelection text =
  let
    elementText element =
      case element of
        Word w ->
          w
        Punctuation p ->
          p
        ParagraphBreak ->
          " "
  in
    case maybeSelection of
      Just (_, start, end) ->
        Dict.filter (\no _ -> no >= start && no <= end) text 
          |> Dict.toList
          |> List.map (\(_, (element, _)) -> elementText element)
          |> String.concat
          |> Just
      Nothing ->
        Nothing


parseRawText : String -> Text
parseRawText rawText =
  find All (regex "([^.,;\"?!\\s]+)|([.,;\"?! \\t]+)|([\\r\\n]+)") rawText
    |> List.map parseRawElement
    |> Dict.fromList


parseRawElement : Regex.Match -> NumberedElementWithMetadata
parseRawElement match =
  let
    element =
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
    (match.number, (element, False))
