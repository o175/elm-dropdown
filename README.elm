import Html exposing (Html, Attribute, div, fieldset, button, input, label, text)
import Html.Attributes exposing (name, style, type_)
import Html.Events exposing (onClick)
import Markdown



main =
  Html.beginnerProgram { model = chapter1, update = update, view = view }



-- MODEL


type alias Model =
  { fontSize : FontSize
  , content : String
  , state : DDState
  , elements : String
  }

type DDState = Opened | Closed

type FontSize
  = Small
  | Medium
  | Large


chapter1 : Model
chapter1 =
  Model Medium intro Closed "hello world!"


intro : String
intro = """

# Anna Karenina

## Chapter 1

Happy families are all alike; every unhappy family is unhappy in its own way.

Everything was in confusion in the Oblonskys’ house. The wife had discovered
that the husband was carrying on an intrigue with a French girl, who had been
a governess in their family, and she had announced to her husband that she
could not go on living in the same house with him...

"""



-- UPDATE


type Msg
  = SwitchTo FontSize | Toggle


update : Msg -> Model -> Model
update msg model =
  case msg of
    SwitchTo newFontSize ->
      { model | fontSize = newFontSize }
    Toggle -> 
      { model | state = if model.state == Opened then Closed else Opened }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ fieldset []
        [ radio "Small" (SwitchTo Small)
        , radio "Medium" (SwitchTo Medium)
        , radio "Large" (SwitchTo Large)
        ]
    , dropdown model.state (List.map (\x -> (String.concat ["Item ", (toString x)])) (List.range 1 10060))
    , Markdown.toHtml [ sizeToStyle model.fontSize ] model.content
    ]

dropdown state elements =
  div[]
    [ dropButton state
      , dropContent state elements        
    ]
    
dropContent state elements = 
  div [ 
        style[ ("position", "absolute") 
             , ("font-size", "0.8em")
             , ("background", "red")
             , ("overflow-y", "scroll")
             , ("height", "500px")
             ]
      ] 
      [ if state == Opened then 
                             div [] (List.map dropElement elements)
                           else 
                             text ""
      ]

dropElement label =
  div [ style [ ("border", "solid")]
      ][ text label ]

dropButton state =
  button [ onClick Toggle] 
         [ text(case state of 
                  Opened -> "Close me!"
                  Closed -> "Open me!"
              )]

radio : String -> msg -> Html msg
radio value msg =
  label
    [ 
      style [ ("padding", "20px") ]
    ]
    [ input [ type_ "radio", name "font-size", onClick msg ] []
    , text value
    ]


sizeToStyle : FontSize -> Attribute msg
sizeToStyle fontSize =
  let
    size =
      case fontSize of
        Small ->
          "0.8em"

        Medium ->
          "1em"

        Large ->
          "1.2em"
  in
    style [("font-size", size)]
