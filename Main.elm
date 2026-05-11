import MiniGame as MiniGame  -- CHANGE MiniGameGroup3 TO OFFICALMiniGame3

myShapes model =
    case model.state of
        Lobby  ->
            [ rect 192 128 |> filled pink,
            text "Lobby"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled red
                  ,    text "Go to Zoo!"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (0, -25)
                     |> notifyTap ToZoo
            ]
        Zoo  ->
            [ rect 192 128 |> filled darkGreen
            , mouse model
                |> move(-30,0)
            , mouse model
                |> move(30,0)
            , text "Zoo"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "Arcade ->"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (25, -25)
                     |> notifyTap ToArc
             , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "<- Lobby"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-25, -25)
                     |> notifyTap ToLobby
            ]
        Arcade subModel  ->
            [ MiniGame.myShapes subModel
                  |> group
                  |> GraphicSVG.map MiniGameMsg
            , text "Arcade"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 60 20 5
                            |> filled green
                  ,    text "<- Zoo"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-65, -50)
                     |> notifyTap LostArc
            , group
                  [
                       roundedRect 60 20 5
                            |> filled green
                  ,    text "BookStore ->"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (65, -50)
                     |> notifyTap ToBookStr
              ]
        BookStore  ->
            [ rect 192 128 |> filled brown
            , text "BookStore"
                  |> centered
                  |> filled black
            , group
                  [    roundedRect 60 20 5
                            |> filled darkBrown
                  ,    text "<- Arcade"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-35, -25)
                     |> notifyTap ToArc
            , group
                  [
                       roundedRect 60 20 5
                            |> filled darkBrown
                  ,    text "Winners Lobby ->"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (35, -25)
                     |> notifyTap ToWinners
            ]
        WinnersLobby  ->
            [ rect 192 128 |> filled yellow
            , text "WinnersLobby"
                  |> centered
                  |> filled black
            , group
                  [    roundedRect 40 20 5
                            |> filled orange
                  ,    text "Go Back"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (0, -25)
                     |> notifyTap ToBookStr
            ]
type Msg = Tick Float GetKeyState
         | ToLobby 
         | ToZoo 
         | ToArc 
         | LostArc 
         | ToBookStr 
         | ToWinners 
         | MiniGameMsg MiniGame.Msg  -- Added this

type State = Lobby 
           | Zoo 
           -- if minigame restart everytime we go there, include it in the state constructor
           | Arcade MiniGame.Model  -- add model code.
           | BookStore 
           | WinnersLobby 

update : Msg -> Model -> Model  -- Added this
update msg model =
  case msg of
    Tick t keys ->
            case model.state of
              Arcade subModel -> 
                  { model | time = t 
                          , state = Arcade (MiniGame.update (MiniGame.Tick t keys) subModel)} -- This logic allows us to use keystrokes for minigame.
              otherwise ->
                    { model | time = t }
    ToLobby  ->
            case model.state of
                Zoo  ->
                    { model | state = Lobby  }

                otherwise ->
                    model
    ToZoo  ->
            case model.state of
                Lobby  ->
                    { model | state = Zoo  }

                otherwise ->
                    model
    ToArc  ->
        case model.state of
            Zoo  ->
                { model | state = Arcade MiniGame.init } -- MiniGame.init makes sure minigame starts over again.

            BookStore ->
                { model | state = Arcade MiniGame.init } -- MiniGame.init makes sure minigame starts over again.

            otherwise ->
                model
    LostArc  ->
        case model.state of
            Arcade subModel ->
                { model | state = Zoo  }

            otherwise ->
                model
    ToBookStr  ->
        case model.state of
            Arcade subModel  ->
                { model | state = BookStore  }

            WinnersLobby  ->
                { model | state = BookStore  }

            otherwise ->
                model
    ToWinners  ->
        case model.state of
            BookStore  ->
                { model | state = WinnersLobby  }

            otherwise ->
                model
    MiniGameMsg subMsg  ->
        case model.state of
            Arcade subModel  ->
                { model | state = Arcade (MiniGame.update subMsg subModel)  }
            otherwise ->
                model
     
type alias Model =
    { time : Float
    , state : State
    }


-- Mouse Image (created with shapecreator)
mouse model = group [
    roundedRect 16 20 5
      |> filled 
          gray
        |> scale 1
        |> move (0,2)
  , circle 10
      |> filled 
          gray
        |> move (0,21)
  , circle 10
      |> filled 
          gray
        |> scale 0.5
        |> move (10,31)
  , circle 10
      |> filled 
          gray
        |> scale 0.5
        |> move (-10,31)
  , circle 10
      |> filled 
          (rgb 255 150 209)
        |> scale 0.25
        |> move (9,29)
  , circle 10
      |> filled 
          (rgb 255 150 209)
        |> scale 0.25
        |> move (-9,29)
  , circle 10
      |> filled 
          black
        |> scale -0.25
        |> move (-3,22)
  , circle 10
      |> filled 
          black
        |> scale -0.25
        |> move (4,22)

 ]
type alias Point = (Float, Float)

init : Model
init = { time = 0 
       , state = Lobby 
       }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)