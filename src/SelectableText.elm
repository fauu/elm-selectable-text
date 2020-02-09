module SelectableText exposing
    ( defaultOptions, Model, initialModel
    , Msg(..)
    , view
    , update
    )

{-| A selectable text component that renders provided text and lets the
user select words using mouse. All the elements of the selection are assigned a
customizable css class. The model contains the selected phrase in form of a
string to be used by the parent component.


# Model

@docs defaultOptions, Model, initialModel


# Messages

@docs Msg


# View function

@docs view


# Update function

@docs update

-}

import Dict exposing (Dict)
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (attribute, class, id)
import Html.Events exposing (onMouseDown, onMouseEnter, onMouseLeave, onMouseUp)
import List
import List.Extra
import Regex
import String
import Tuple



-- MODEL


type alias Options =
    { id : String
    , selectedElementClass : String
    , placeholderText : String
    , allowInterparagraphSelection : Bool
    , maxSelectionLength : Maybe Int
    }


{-| The component model. The `selectedPhrase` field contains the selected phrase
as a string which can be read by the parent component.
-}
type alias Model =
    { text : Text
    , mouseOverWordNo : Maybe ElementNo
    , selecting : Bool
    , selection : Maybe Selection
    , selectedPhrase : Maybe String
    , options : Options
    }


type Element
    = Word String
    | Punctuation String
    | ParagraphBreak


type alias SelectionIndicator =
    Bool


type alias ElementWithMetadata =
    ( Element, SelectionIndicator )


type alias ElementNo =
    Int


type alias NumberedElementWithMetadata =
    ( ElementNo, ElementWithMetadata )


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
    ( ElementNo, ElementNo, ElementNo )


{-| Default component options:

  - **id** = "text" - The HTML id attribute for the root tag
  - **selectedElementClass** = "selected" - The CSS class of the selected
    elements
  - **placeholderText** = "" - A string that will be displayed until a text is
    provided by the parent using the `RenderText` message
  - **allowInterparagraphSelection** = True - Whether to allow the selection to
    span across multiple paragraphs
  - **maxSelectionLength** = Nothing - Maximal selection length in words

-}
defaultOptions : Options
defaultOptions =
    { id = "text"
    , selectedElementClass = "selected"
    , placeholderText = ""
    , allowInterparagraphSelection = True
    , maxSelectionLength = Nothing
    }


{-| Initializes the component. Expects user to provide a `defaultOptions`
record with desired extensions.

    import SelectableText exposing (defaultOptions)

    selectableTextModel =
        SelectableText.initialModel
            { defaultOptions
                | id = "my-text"
                , placeholderText = "Loading..."
                , allowInterparagraphSelection = False
            }

-}
initialModel : Options -> Model
initialModel options =
    { text = Dict.singleton 0 ( Word options.placeholderText, False )
    , mouseOverWordNo = Nothing
    , selecting = False
    , selection = Nothing
    , selectedPhrase = Nothing
    , options = options
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
view { text, options } =
    let
        paragraphs =
            List.map (viewParagraph options) (Dict.toList text |> splitIntoParagraphs)
    in
    div
        [ id options.id
        , onMouseDown StartSelecting
        , onMouseUp StopSelecting
        ]
        paragraphs


splitIntoParagraphs : List NumberedElementWithMetadata -> List Paragraph
splitIntoParagraphs elements =
    let
        isNotParagraphBreak ( _, ( element, _ ) ) =
            not <| isParagraphBreak element
    in
    (::) (List.Extra.takeWhile isNotParagraphBreak elements) <|
        let
            maybeTail =
                List.tail <|
                    List.Extra.dropWhile isNotParagraphBreak elements
        in
        case maybeTail of
            Just tail ->
                splitIntoParagraphs tail

            Nothing ->
                []


viewParagraph : Options -> Paragraph -> Html Msg
viewParagraph options paragraph =
    p [] (List.map (viewElement options) paragraph)


viewElement : Options -> NumberedElementWithMetadata -> Html Msg
viewElement { selectedElementClass } ( no, ( element, isSelected ) ) =
    case element of
        Word w ->
            let
                classNode =
                    if isSelected then
                        [ class selectedElementClass ]

                    else
                        []

                attributes =
                    [ onMouseEnter (MouseEnteredWord no)
                    , onMouseLeave (MouseLeftWord no)
                    ]
                        ++ classNode
            in
            span attributes [ text w ]

        Punctuation p ->
            if isSelected then
                span [ class selectedElementClass ] [ text p ]

            else
                text p

        ParagraphBreak ->
            text ""



-- UPDATE


{-| The selectable text update function.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        StartSelecting ->
            let
                newSelection =
                    case model.mouseOverWordNo of
                        Just no ->
                            Just ( no, no, no )

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
            ( newModel
            , Cmd.none
            )

        StopSelecting ->
            ( { model
                | selecting = False
                , selectedPhrase = selectedPhrase model.selection model.text
              }
            , Cmd.none
            )

        MouseEnteredWord no ->
            let
                newSelection =
                    if model.selecting then
                        recalculateSelection
                            model.options
                            no
                            model.text
                            model.selection

                    else
                        model.selection

                newText =
                    if model.selecting then
                        markSelection newSelection model.text

                    else
                        model.text

                newModel =
                    { model
                        | mouseOverWordNo = Just no
                        , selection = newSelection
                        , text = newText
                    }
            in
            ( newModel
            , Cmd.none
            )

        MouseLeftWord no ->
            ( { model | mouseOverWordNo = Nothing }
            , Cmd.none
            )

        RenderText rawText ->
            ( { model | text = parseRawText rawText }
            , Cmd.none
            )


isWord : Element -> Bool
isWord element =
    case element of
        Word _ ->
            True

        _ ->
            False


isParagraphBreak : Element -> Bool
isParagraphBreak element =
    case element of
        ParagraphBreak ->
            True

        _ ->
            False


elementsFromRange : ElementNo -> ElementNo -> Text -> Text
elementsFromRange start end =
    Dict.filter (\no _ -> no >= start && no <= end)


sort2 : comparable -> comparable -> ( comparable, comparable )
sort2 a b =
    ( min a b, max a b )


{-| Recalculates the selection given a new element number
-}
recalculateSelection :
    Options
    -> ElementNo
    -> Text
    -> Maybe Selection
    -> Maybe Selection
recalculateSelection { allowInterparagraphSelection, maxSelectionLength } newNo text maybeSelection =
    case maybeSelection of
        (Just ( initialNo, _, _ )) as currentSelection ->
            let
                hasParagraphBreaks =
                    Dict.foldl
                        (\_ ( element, _ ) acc -> acc || isParagraphBreak element)
                        False

                countWords =
                    Dict.foldl
                        (\_ ( element, _ ) acc ->
                            acc
                                + (if isWord element then
                                    1

                                   else
                                    0
                                  )
                        )
                        0

                paragraphBreakBetween startNo endNo =
                    text
                        |> elementsFromRange startNo endNo
                        |> hasParagraphBreaks

                tentativeRange =
                    sort2 initialNo newNo

                goesOutsideParagraph =
                    (\( a, b ) -> paragraphBreakBetween a b) tentativeRange

                exceedsLength length =
                    text
                        |> (\( a, b ) -> elementsFromRange a b) tentativeRange
                        |> countWords
                        |> (<) length

                exceedsMaxLength =
                    case maxSelectionLength of
                        Just length ->
                            exceedsLength length

                        Nothing ->
                            False
            in
            if
                (allowInterparagraphSelection || not goesOutsideParagraph)
                    && not exceedsMaxLength
            then
                Just ( initialNo, Tuple.first tentativeRange, Tuple.second tentativeRange )

            else
                currentSelection

        Nothing ->
            Just ( newNo, newNo, newNo )


{-| Given a selection, sets the selection indicator for the appropriate text
elements.
-}
markSelection : Maybe Selection -> Text -> Text
markSelection maybeSelection text =
    let
        newIsSelected no =
            case maybeSelection of
                Just ( _, start, end ) ->
                    no >= start && no <= end

                Nothing ->
                    False
    in
    Dict.map
        (\no ( element, isSelected ) -> ( element, newIsSelected no ))
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
        Just ( _, startNo, endNo ) ->
            text
                |> elementsFromRange startNo endNo
                |> Dict.toList
                |> List.map (\( _, ( element, _ ) ) -> elementText element)
                |> String.concat
                |> Just

        Nothing ->
            Nothing


somethingParser : Regex.Regex
somethingParser =
  Maybe.withDefault Regex.never <|
    Regex.fromString "([^.,;\"'?!«»\\s\\t]+)|([.,;\"'?!«» \\t]+)|([\\r\\n]+)"

parseRawText : String -> Text
parseRawText rawText =
    Regex.find somethingParser rawText
        |> List.map parseRawElement
        |> Dict.fromList


parseRawElement : Regex.Match -> NumberedElementWithMetadata
parseRawElement match =
    let
        element =
            case match.submatches of
                [ Just word, _, _ ] ->
                    Word word

                [ _, Just punctuaction, _ ] ->
                    Punctuation punctuaction

                [ _, _, Just paragraphBreak ] ->
                    ParagraphBreak

                _ ->
                    Punctuation " "
    in
    ( match.number, ( element, False ) )
