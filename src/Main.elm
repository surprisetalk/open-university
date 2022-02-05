
-- MODULE ---------------------------------------------------------------------

module Main exposing (..)


-- IMPORTS --------------------------------------------------------------------

import Dict exposing (Dict)
import Browser
import Html exposing (Html, div, text, p, a, ul, li, h1, h2, button, input, br, hr)
import Html.Attributes as Attr exposing (id, class, href)
import Html.Events as Event exposing (onClick, onInput)
import Regex exposing (Regex)
import Http
import Json.Decode as D
import Json.Encode as E
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>))
import Url.Builder as UrlBuilder
import Markdown


-- MAIN -----------------------------------------------------------------------

main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlRequest = UrlRequested
    , onUrlChange = UrlChanged
    }


-- MODEL ----------------------------------------------------------------------

type alias Model
  = { key : Nav.Key
    , lessons : Loadable (List Lesson)
    , guide : Maybe Guide
    , puzzle : Maybe Puzzle
    }

type Loadable a
  = Loading
  | Loaded a
  | Error Http.Error

type Lesson
  = Lesson
    { typ : LessonType
    , title : String
    , hash : String
    , lessons : List Lesson
    }

type LessonType
  = LessonLesson
  | LessonGuide
  | LessonPuzzle

type alias Guide
  = { title : String
    , hash : String
    , content : Html Msg
    }

type alias Puzzle
  = { title : String
    , hash : String
    , current : List (QA Guess)
    , history : List (NonEmptyList (QA Solution))
    }

type alias QA a
  = { question : Html Msg
    , answer : a
    }

type Guess
  = TextGuess Regex String
  | ChoiceGuess (List (Html Msg)) Int

type Solution
  = TextSolution
    { guess : String
    , solution : String
    }
  | ChoiceSolution
    { choices : List (Html Msg)
    , guess : Int
    , solution : Int
    }

type alias NonEmptyList a
  = (a, List a)


-- INIT -----------------------------------------------------------------------

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  ( { key = key
    , lessons = Loading
    , guide = Nothing
    , puzzle = Nothing
    }
  , Cmd.batch
    [ route url
      |> Maybe.withDefault Cmd.none
    , Http.get
      { url = "/api/lessons"
      , expect = Http.expectJson LessonsFetched (D.list lessonDecoder)
      }
    ]
  )

lessonDecoder =
  D.map4 (\ typ title hash lessons -> Lesson { typ = typ, title = title, hash = hash, lessons = lessons })
  (D.field "type" D.string |> D.andThen lessonTypeDecoder)
  (D.field "title" D.string)
  (D.field "hash" D.string)
  (D.maybe (D.field "lessons" (D.list (D.lazy (\ _ -> lessonDecoder)))) |> D.map (Maybe.withDefault []))

lessonTypeDecoder typ =
  case typ of
    "lesson" -> D.succeed LessonLesson
    "guide" -> D.succeed LessonGuide
    "puzzle" -> D.succeed LessonPuzzle
    _ -> D.fail "TODO"

guideDecoder =
  D.map3 Guide
  (D.field "title" D.string)
  (D.field "hash" D.string)
  (D.field "content" markdownDecoder)

puzzleDecoder =
  D.map4 Puzzle
  (D.field "title" D.string)
  (D.field "hash" D.string)
  (D.field "current" (D.maybe (D.list qaGuessDecoder) |> D.map (Maybe.withDefault [])))
  (D.field "history" (D.list (nonEmptyListDecoder qaSolutionDecoder)))

qaGuessDecoder =
  D.map2 QA
  (D.field "question" markdownDecoder)
  (D.field "choices" guessDecoder)

guessDecoder
  = D.list markdownDecoder
    |> D.map
       (\ answer -> case answer of
          [] -> TextGuess Regex.never ""
          choices -> ChoiceGuess choices 0
       )

qaSolutionDecoder =
  D.map2 QA
  (D.field "question" markdownDecoder)
  ( D.oneOf
    [ D.map3 (\ choices guess solution -> ChoiceSolution { choices = choices, guess = guess, solution = solution })
      (D.field "choices" (D.list markdownDecoder))
      (D.field "guess" intParser)
      (D.field "solution" intParser)
    , D.map2 (\ guess solution -> TextSolution { guess = guess, solution = solution })
      (D.field "guess" stringParser)
      (D.field "solution" stringParser)
    ]
  )

intParser = D.oneOf
  [ D.map String.toInt D.string
    |> D.andThen (Maybe.map D.succeed >> Maybe.withDefault (D.fail "TODO"))
  , D.int
  ]

stringParser = D.oneOf
  [ D.string
  , D.int |> D.map String.fromInt
  , D.float |> D.map String.fromFloat
  ]

nonEmptyListDecoder p
  = D.list p
    |> D.andThen
       (\ xs -> case xs of
          [] -> D.fail "empty list"
          y :: ys -> D.succeed (y,ys)
       )

markdownDecoder = D.string |> D.map (Markdown.toHtmlWith Markdown.defaultOptions [class "markdown"])


-- UPDATE ---------------------------------------------------------------------

type Msg
  = NoOp
  | UrlRequested Browser.UrlRequest
  | UrlChanged Url
  | LessonsFetched (Result Http.Error (List Lesson))
  | GuideFetched (Result Http.Error Guide)
  | PuzzleFetched (Result Http.Error Puzzle)
  | PuzzleRequested
  | PuzzleRequestFinished String (Result Http.Error ())
  | PuzzleSubmitted
  | PuzzleSubmissionFinished String (Result Http.Error ())
  | GuessedText Int String
  | GuessedChoice Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    UrlRequested (Browser.Internal url) ->
      case route url of
        Nothing -> ({ model | guide = Nothing, puzzle = Nothing }, Nav.pushUrl model.key "/")
        Just cmd -> (model, Cmd.batch [cmd, Nav.pushUrl model.key (Url.toString url)])
    UrlRequested (Browser.External url) ->
      (model, Nav.load url)
    UrlChanged _ ->
      (model, Cmd.none)
    LessonsFetched (Ok lessons) ->
      ({ model | lessons = Loaded lessons }, Cmd.none)
    LessonsFetched (Err error) ->
      ({ model | lessons = Error error }, Cmd.none)
    GuideFetched (Ok guide) ->
      ({ model | guide = Just guide }, Cmd.none)
    GuideFetched (Err error) ->
      ({ model | lessons = Error error }, Cmd.none)
    PuzzleFetched (Ok puzzle) ->
      ({ model | puzzle = Just puzzle }, Cmd.none)
    PuzzleFetched (Err error) ->
      ({ model | lessons = Error error }, Cmd.none)
    PuzzleRequested ->
      ( model
      , case model.puzzle of
          Nothing ->
            Cmd.none
          Just {hash} ->
            Http.post
            { url = "/api/puzzle/" ++ hash
            , body = Http.stringBody "text/plain" ""
            , expect = Http.expectWhatever (PuzzleRequestFinished hash)
            }
      )
    PuzzleRequestFinished hash (Ok ()) ->
      ( model
      , Http.get
        { url = "/api/puzzle/" ++ hash
        , expect = Http.expectJson PuzzleFetched puzzleDecoder
        }
      )
    PuzzleRequestFinished hash (Err error) ->
      ({ model | lessons = Error error }, Cmd.none)
    PuzzleSubmitted ->
      ( model
      , case model.puzzle of
          Nothing ->
            Cmd.none
          Just {hash,current} ->
            Http.post
            { url
              = "/api/puzzle/" ++ hash
            , body
              = Http.jsonBody
                ( E.list
                  (\ qa -> case qa.answer of
                     TextGuess _ input -> E.string input
                     ChoiceGuess _ selected -> E.int selected
                  )
                  current
                )
            , expect
              = Http.expectWhatever (PuzzleRequestFinished hash)
            }
      )
    GuessedText i input ->
      ( { model
          | puzzle = model.puzzle
            |> Maybe.map
               (\ puzzle ->
                  { puzzle
                    | current = puzzle.current
                      |> List.indexedMap
                         (\ j qa ->
                            if j == i
                            then { qa | answer = TextGuess Regex.never input }
                            else qa
                         )
                  }
               )
        }
      , Cmd.none
      )
    GuessedChoice i newSelected ->
      ( { model
          | puzzle = model.puzzle
            |> Maybe.map
               (\ puzzle ->
                  { puzzle
                    | current = puzzle.current
                      |> List.indexedMap
                         (\ j qa ->
                            if j == i
                            then 
                              case qa.answer of
                                ChoiceGuess choices selected -> { qa | answer = ChoiceGuess choices newSelected }
                                _ -> qa
                            else qa
                         )
                  }
               )
        }
      , Cmd.none
      )
    PuzzleSubmissionFinished _ _ ->
      (model, Cmd.none)

route : Url -> Maybe (Cmd Msg)
route = UrlParser.parse <|
  UrlParser.oneOf
  [ UrlParser.s "guide" </> UrlParser.string
    |> UrlParser.map
       (\ hash -> Http.get
          { url = "/api/guide/" ++ hash
          , expect = Http.expectJson GuideFetched guideDecoder
          }
       )
  , UrlParser.s "puzzle" </> UrlParser.string
    |> UrlParser.map
       (\ hash -> Http.get
          { url = "/api/puzzle/" ++ hash
          , expect = Http.expectJson PuzzleFetched puzzleDecoder
          }
       )
  ]


-- SUBSCRIPTIONS --------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW -----------------------------------------------------------------------

-- TODO: Use artcles/sections to space out sections of page.

view : Model -> Browser.Document Msg
view model
  = { title
      = "TODO"
    , body
      = [ Html.header [] [ a [ href "/" ] [ text "home" ] ]
        , case (model.puzzle, model.guide, model.lessons) of
           (Just puzzle, _, _) -> viewPuzzle puzzle
           (_, Just guide, _) -> viewGuide guide
           (_, _, Loaded lessons) -> div [ id "lessons" ] <| viewLessons lessons
           x -> div [] [ text (Debug.toString x) ]
        ]
    }

viewLessons = List.map viewLesson >> ul [] >> List.singleton

viewLesson : Lesson -> Html Msg    
viewLesson (Lesson {typ,title,hash,lessons}) = case typ of
  LessonLesson -> li [] (a [] [text title] :: viewLessons lessons)
  LessonGuide -> li [] [a [href ("/guide/" ++ hash)] [text title]]
  LessonPuzzle -> li [] [a [href ("/puzzle/" ++ hash)] [text title]]

viewGuide ({title,hash,content}) =
  div [ id "guide" ]
  [ h1 [] [ text title ]
  , content
  ]
  
viewPuzzle : Puzzle -> Html Msg
viewPuzzle {title,hash,current,history} =
  div [ id "puzzle" ]
  [ h1 [] [ text title ]
  , div [ id "current" ] <| case current of
      [] ->
        [ button [ onClick PuzzleRequested ] [ text "Start" ]
        , br [] []
        , if List.length history > 0
          then 
            div [ id "history" ]
            <| (::) (h2 [] [ text "History" ])
            <| List.intersperse (br [] [])
            <| List.map (div [] << List.map (viewQA viewSolution))
            <| List.map (\ (x,xs) -> x::xs)
            <| history
          else br [] []
        ]
      qas ->
        [ div [] <| List.indexedMap (\ i qa -> viewQA (viewGuess i) qa) <| qas
        , br [] []
        , button [ onClick PuzzleSubmitted ] [ text "Submit" ]
        ]
  ]

viewQA : (a -> Html Msg) -> QA a -> Html Msg
viewQA viewA {question,answer} =
  div [ class "qa" ]
  [ question
  , viewA answer
  ]

viewGuess : Int -> Guess -> Html Msg
viewGuess i guess = div [ id "guess" ] <| List.singleton <| case guess of
  TextGuess validation input ->
    Html.input [ Attr.value input, onInput (GuessedText i) ] []
  ChoiceGuess choices selected -> 
    div []
    <| List.indexedMap
       (\ selection choice ->
          button
          [ class "choice"
          , if selection == selected
            then class "selected"
            else class "unselected"
          , onClick (GuessedChoice i selection)
          ]
          [ choice
          ]
       )
    <| choices
  

viewSolution : Solution -> Html Msg
viewSolution answer = div [ id "solution" ] <| case answer of
  TextSolution {guess,solution} ->
    [ p [] [ text <| "Your guess: " ++ guess ]
    , p [] [ text <| "Solution: " ++ solution ]
    ]
  ChoiceSolution {choices,guess,solution} ->
    [ div [] <| List.map (\ choice -> button [ class "choice", Attr.disabled True ] [ choice ]) choices
    , p [] [ text <| "Your guess: " ++ String.fromInt guess ]
    , p [] [ text <| "Solution: " ++ String.fromInt solution ]
    ]
  
