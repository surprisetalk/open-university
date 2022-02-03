
-- MODULE ---------------------------------------------------------------------

module Main exposing (..)


-- IMPORTS --------------------------------------------------------------------

import Dict exposing (Dict)
import Browser
import Html exposing (Html, div, text, p, a, ul, li)
import Html.Attributes as Attr exposing (href)
import Regex exposing (Regex)
import Http
import Json.Decode as D
import Browser.Navigation as Nav
import Url exposing (Url)


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
    , content : Html ()
    }

type alias Puzzle
  = { title : String
    , hash : String
    , current : Maybe (List { question : Html (), answer : Answer })
    , history : List (List { question : Html (), answer : Html () })
    }

type alias Title = String

type Answer
  = Int
    { input : String
    , solution : Maybe Int
    , fudge : Float
    }
  | Float
    { input : String
    , solution : Maybe Float
    , fudge : Float
    }
  | Text
    { validation : Regex
    , input : String
    , solution : Maybe String
    }
  | MultipleChoice
    { options : List (Html ())
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
    { url = "http://localhost:3000/lessons"
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

-- [                                                                               
--   {                                                                                      
--     "title": "Test Lesson",                                                              
--     "hash": "cb7e4ec8654799ccbed3b1c7c9a3ca09",                                          
--     "lessons": [                                                                         
--       {                                                                                  
--         "title": "Test Guide",                                                           
--         "hash": "6f5902ac237024bdd0c176cb93063dc4",                                      
--         "type": "guide"                                                                  
--       },                                                                                 
--       {                                                                                  
--         "title": "Test Puzzle",                                                          
--         "hash": "5b6a641e2b358cb6507f367d4cb8392f",                                      
--         "type": "puzzle"                                                                 
--       }                                                                                  
--     ],                                                                                   
--     "type": "lesson"                                                                     
--   }                                                                                      
-- ]
  

-- UPDATE ---------------------------------------------------------------------

type Msg
  = NoOp
  | UrlRequested Browser.UrlRequest
  | UrlChanged Url
  | LessonsFetched (Result Http.Error (List Lesson))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    UrlRequested (Browser.Internal url) ->
      -- TODO: If url starts with "/guide", start loading the guide, etc.
      (model, Nav.pushUrl model.key (Url.toString url))
    UrlRequested (Browser.External url) ->
      (model, Nav.load url)
    LessonsFetched (Ok lessons) ->
      ({ model | lessons = Loaded lessons }, Cmd.none)
    LessonsFetched (Err error) ->
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
      = case (model.puzzle, model.guide, model.lessons) of
          (_, _, Loaded lessons) -> viewLessons lessons
          x -> [ text (Debug.toString x) ]
    }

viewLessons = List.map viewLesson >> ul [] >> List.singleton

viewLesson : Lesson -> Html Msg    
viewLesson (Lesson {typ,title,hash,lessons}) = case typ of
  LessonLesson -> li [] (a [] [text title] :: viewLessons lessons)
  LessonGuide -> li [] [a [href ("/guide/" ++ hash)] [text title]]
  LessonPuzzle -> li [] [a [href ("/puzzle/" ++ hash)] [text title]]
