
-- MODULE ---------------------------------------------------------------------

module Main exposing (..)


-- IMPORTS --------------------------------------------------------------------

import Dict exposing (Dict)
import Browser
import Html exposing (Html, div, text, p)
import Html.Attributes as Attr
import Regex exposing (Regex)
import Http
import Json.Decode as D


-- MAIN -----------------------------------------------------------------------

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL ----------------------------------------------------------------------

type alias Model
  = { lessons : Loadable (List Lesson)
    , guide : Maybe Guide
    , puzzle : Maybe Puzzle
    }

type Loadable a
  = Loading
  | Loaded a
  | Error Http.Error

type Lesson
  = Lesson
    { title : String
    , hash : String
    , lessons : List Lesson
    }

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

init : () -> (Model, Cmd Msg)
init _ =
  ( { lessons = Loading
    , guide = Nothing
    , puzzle = Nothing
    }
  , Http.get
    { url = "https://localhost:3000/lessons"
    , expect = Http.expectJson LessonsUpdated lessonsDecoder
    }
  )

lessonsDecoder = D.fail "TODO"
  

-- UPDATE ---------------------------------------------------------------------

type Msg
  = NoOp
  | LessonsUpdated (Result Http.Error (List Lesson))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    _ ->
      (model, Cmd.none)



-- SUBSCRIPTIONS --------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW -----------------------------------------------------------------------

view : Model -> Html Msg
view model =
  case (model.puzzle, model.guide, model.lessons) of
    _ -> text "hello world"
    
