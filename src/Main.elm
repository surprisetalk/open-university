
-- MODULE ---------------------------------------------------------------------

module Main exposing (..)


-- IMPORTS --------------------------------------------------------------------

import Dict exposing (Dict)
import Browser
import Html exposing (Html, div, text, p)
import Html.Attributes as Attr
import Regex exposing (Regex)


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
  = { lessons : List Lesson
    }

type Lesson
  = Lesson
    { lessons : List Lesson
    }
  | Guide
    { content : Html ()
    }
  | Puzzle
    { current : List { question : Html (), answer : Answer }
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
  ( { lessons
      = [ Puzzle
          { current
            = [ { question = text "When \\(a \\ne 0\\), there are two solutions to \\(ax^2 + bx + c = 0\\) and they are \\[x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.\\]"
                , answer = Text { validation = Regex.never, input = "", solution = Nothing }
                }
              ]
          , history = []
          }
        ]
    }
  , Cmd.none
  )


-- UPDATE ---------------------------------------------------------------------

type Msg
  = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)


-- SUBSCRIPTIONS --------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW -----------------------------------------------------------------------

view : Model -> Html Msg
view model =
  model.lessons
  |> List.concatMap
     ( \ lesson ->
         case lesson of
           Puzzle {current} ->
             current
             |> List.map (\ {question} -> div [] [ question ])
           _ -> [ div [] [ text "TODO" ] ]
     )
  |> div []
  |> Html.map (\_ -> NoOp)
