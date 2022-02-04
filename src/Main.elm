
-- MODULE ---------------------------------------------------------------------

module Main exposing (..)


-- IMPORTS --------------------------------------------------------------------

import Dict exposing (Dict)
import Browser
import Html exposing (Html, div, text, p, a, ul, li, h1, h2, button)
import Html.Attributes as Attr exposing (href)
import Html.Events as Event exposing (onClick)
import Regex exposing (Regex)
import Http
import Json.Decode as D
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

-- BUG: current = Just []
-- BUG: history = [[]]
type alias Puzzle
  = { title : String
    , hash : String
    , current : Maybe (List (QA Answer))
    , history : List (List (QA (Html Msg)))
    }

type alias QA a
  = { question : Html Msg
    , answer : a
    }

type alias Title = String

type Answer
  = Int
    { input : String
    , solution : Maybe Int
    }
  | Float
    { input : String
    , solution : Maybe Float
    }
  | Text
    { validation : Regex
    , input : String
    , solution : Maybe String
    }
  | MultipleChoice
    { options : List (Html Msg)
    , selected : Int
    , solution : Maybe Int
    }


-- INIT -----------------------------------------------------------------------

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ _ key =
  ( { key = key
    , lessons = Loading
    , guide = Nothing
    , puzzle = Nothing
    }
  , Http.get
    { url = "/api/lessons"
    , expect = Http.expectJson LessonsFetched (D.list lessonDecoder)
    }
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

-- {                                                                                                              
--   "6f5902ac237024bdd0c176cb93063dc4": {                                                                                
--     "type": "guide",                                                                                                   
--     "title": "Test Guide",                                                                                             
--     "content": "hello world\n",                                                                                        
--     "hash": "6f5902ac237024bdd0c176cb93063dc4"                                                                         
--   }                                                                                                                    
-- }   

guideDecoder =
  D.map3 Guide
  (D.field "title" D.string)
  (D.field "hash" D.string)
  (D.field "content" markdownDecoder)

puzzleDecoder =
  D.map4 Puzzle
  (D.field "title" D.string)
  (D.field "hash" D.string)
  (D.field "current" (D.maybe (D.list (qaDecoder optionsDecoder))))
  (D.field "history" (D.list (D.list (qaDecoder markdownDecoder))))

qaDecoder p =
  D.map2 QA
  (D.field "question" markdownDecoder)
  (D.field "options" p)

optionsDecoder
  = D.oneOf
    [ D.string
      |> D.andThen
         (\ x -> case x of
            "INT" -> D.succeed
              <| Int
                 { input = ""
                 , solution = Nothing
                 }
            _ -> D.fail "TODO"
         )
    ]

markdownDecoder = D.string
  |> D.map (Markdown.toHtmlWith Markdown.defaultOptions [])

  

-- UPDATE ---------------------------------------------------------------------

type Msg
  = NoOp
  | UrlRequested Browser.UrlRequest
  | UrlChanged Url
  | LessonsFetched (Result Http.Error (List Lesson))
  | GuideFetched (Result Http.Error Guide)
  | PuzzleFetched (Result Http.Error Puzzle)
  | PuzzleRequested String
  | PuzzleRequestFinished String (Result Http.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    UrlRequested (Browser.Internal url) ->
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
      |> UrlParser.map (Tuple.pair url)
      |> (\ p -> UrlParser.parse p url)
      |> Maybe.withDefault (Url.fromString "/" |> Maybe.withDefault url, Cmd.none)
      |> (\ (newUrl, cmd) -> (model, Cmd.batch [cmd, Nav.pushUrl model.key (Url.toString newUrl)]))
    UrlRequested (Browser.External url) ->
      (model, Nav.load url)
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
    PuzzleRequested hash ->
      ( model
      , Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/puzzle/" ++ hash
        , body = Http.emptyBody
        , expect = Http.expectWhatever (PuzzleRequestFinished hash)
        , timeout = Nothing
        , tracker = Nothing
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
    _ ->
      (model, Cmd.none)



-- SUBSCRIPTIONS --------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW -----------------------------------------------------------------------

view : Model -> Browser.Document Msg
view model
  = { title
      = "TODO"
    , body
      = a [href "/"] [text "home"]
        :: case (model.puzzle, model.guide, model.lessons) of
             (Just puzzle, _, _) -> viewPuzzle puzzle
             (_, Just guide, _) -> viewGuide guide
             (_, _, Loaded lessons) -> viewLessons lessons
             x -> [ text (Debug.toString x) ]
    }

viewLessons = List.map viewLesson >> ul [] >> List.singleton

viewLesson : Lesson -> Html Msg    
viewLesson (Lesson {typ,title,hash,lessons}) = case typ of
  LessonLesson -> li [] (a [] [text title] :: viewLessons lessons)
  LessonGuide -> li [] [a [href ("/guide/" ++ hash)] [text title]]
  LessonPuzzle -> li [] [a [href ("/puzzle/" ++ hash)] [text title]]

viewGuide ({title,hash,content}) =
  [ h1 [] [ text title ]
  , h2 [] [ text hash ]
  , div [] [ content ]
  ]
  
viewPuzzle : Puzzle -> List (Html Msg)
viewPuzzle {title,hash,current,history} =
  [ h1 [] [ text title ]
  , h2 [] [ text hash ]
  , case current of
      Nothing -> button [ onClick (PuzzleRequested hash) ] [ text "Start" ]
      Just [] -> button [ onClick (PuzzleRequested hash) ] [ text "Start" ]
      Just qas -> qas |> List.map (\{question} -> question) |> div []
  ]
  
