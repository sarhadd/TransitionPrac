module MiniGame exposing (..)
import GraphicSVG.App exposing (..)

-- In this game, we use notifyTap, introduced in Blotter.elm to nudge a ball in different
-- directions.  The goal is to put the ball in the goal, and not kick it off the field.

init : Model
init = { time = 0, position = (0,0), velocity = (7,3), goals = 0, state = Playing }

myShapes : Model -> List (Shape Msg)
myShapes model =
   case model.state of
     Scored -> 
       [ text ("You Win!!!")
                   |> centered
                   |> filled orange
                   |> move (10 * sin (7*model.time), 8 * sin (13*model.time))
               ]
     Playing -> 
       [ text ("Tap to nudge your ball, to move on!")
                 |> filled orange
                 |> move (-85,40)
             ]
       ++
       -- the goal
       [ rect 20 60 |> outlined (dotted 1) green |> move (-85,0)
       , rect 20 56 |> filled white |> move (-83,0)
       -- the ball
       , circle 10 |> filled red |> move model.position |> notifyTapAt TapAt ]
   --otherwise ->


inside (x,y) = x > -110 && x < 110 && y < 72 && y > -72
type State
    = Playing
    | Scored
    
-- For anygame it usually always has this.
type Msg = Tick Float GetKeyState
         | TapAt (Float,Float)
         | StartAgain
                
-- { time : Float, position : (Float,Float), velocity : (Float,Float), goals : Int }
type alias Model = 
          { position : ( Float, Float )
          , time : Float
          , velocity : ( Float, Float )
          , goals : Int
          , state : State
          }

update : Msg -> Model -> Model
update msg model = case msg of
                     -- now when time goes by, we have to test the position of the
                     -- ball to see if it goes into the goal, and if not if it
                     -- goes out of bounds
                     Tick t _ -> if inGoal model.position
                                   then
                                     { model | time = t
                                             , position = (40,0)
                                             , velocity = (-3,8)
                                             , goals = model.goals + 1
                                             , state = Scored }
                                   else
                                     if inside model.position
                                       then
                                         { model | time = t
                                                 , position = moveBall model.velocity model.position }
                                       else
                                         -- We Bounce back in range. somehow
                                         let
                                           -- Current location
                                           (x, y) = 
                                               model.position
                                           (vx, vy) = 
                                               model.velocity
                                               
                                           -- Bounce back effect
                                           newVx =
                                                if x <= -110 || x >= 110 then -vx else vx
                                           newVy =
                                                if y <= -72 || y >= 72 then -vy else vy
                                         in
                                         { model | time = t
                                                 , position = moveBall (newVx,newVy) model.position, velocity = (newVx,newVy)
                                                 -- goals = model.goals - 1  WE DO NOT NEED NEGATIVE POINTS!
                                         }
                     StartAgain -> { model | goals = 0, position = (0,0), velocity = (3,-4) }
                     TapAt tap -> { model | velocity = nudge tap model.position model.velocity }


-- we are using the same logic here that we used in KickBall.elm.
-- && means both sides have to be true
-- since we only use && for "and" and not || for "or", we don't need ()s
inGoal (x,y) = x < -85 && y < 15 && y > -15

moveBall (vx,vy) (x,y) = (x + vx/50, y + vy/50) -- Play with this scale to adjust ball speed.

nudge (tapx,tapy) (x,y) (vx,vy) =
  (vx + (x - tapx), vy + (y - tapy))


-- Last two lines are kept.
main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view : Model -> Collage Msg
view model = collage 192 128 (myShapes model)




