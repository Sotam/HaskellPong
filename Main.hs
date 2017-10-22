module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> PongGame -> PongGame 
update seconds game | x' >=   150      = error "Player 1 wins"
                    | x' <= (-150)     = error "Player 2 wins"
                    | state == Running = paddleBounce (wallBounce (moveBall seconds (keypress game)))
                    | otherwise        = id game
                     where state = gameState game
                           (x', _) = ballLoc game

keypress :: PongGame -> PongGame
keypress = (upOrDown "p1") . (upOrDown "p2")

keypress' :: String -> PongGame -> PongGame
keypress' player game | state == Down && player == "player1" = game { player1 = (player1 game) + 1 }
                      | state == Down && player == "player2" = game { player2 = (player2 game) + 1 }
                      | otherwise                            = game 
                      where 
                         state | player == "player1" = player1Up game
                               | otherwise           = player2Up game 

upOrDown player game | player == "p1" && p1UpMogelijk && (player1Up game) == Down = game { player1 = (player1 game) + 1 }
                     | player == "p1" && p1DoMogelijk && (player1Do game) == Down = game { player1 = (player1 game) - 1 }
                     
                     | player == "p2" && p2UpMogelijk && (player2Up game) == Down = game { player2 = (player2 game) + 1 }
                     | player == "p2" && p2DoMogelijk && (player2Do game) == Down = game { player2 = (player2 game) - 1 }
                     | otherwise                                                  = game
                     where p1UpMogelijk = player1 game <= 100
                           p1DoMogelijk = player1 game >= (-100)

                           p2UpMogelijk = player2 game <= 100
                           p2DoMogelijk = player2 game >= (-100)

data GameState = Running | Paused
    deriving (Show, Eq)

-- | Data describing the state of the pong game. 
data PongGame  = Game
  { ballLoc   :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel   :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , player1   :: Float           -- ^ Left player paddle height.
  , player1Up :: KeyState
  , player1Do :: KeyState
                               -- Zero is the middle of the screen. 
  , player2   :: Float           -- ^ Right player paddle height.
  , player2Up :: KeyState
  , player2Do :: KeyState
  , gameState :: GameState
  } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState  = Game
  { ballLoc   = (0, 0)
  , ballVel   = (10, -3) -- * 1, -3
  , player1   = 0
  , player1Up = Up
  , player1Do = Up
  , player2   = 0
  , player2Up = Up
  , player2Do = Up
  , gameState = Running
  }

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds * 5
    y' = y + vy * seconds * 5

type Radius = Float 
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy

paddleCollision :: Position -> Float -> Float -> Bool
paddleCollision (x, y) player height = leftCollision || rightCollision
    where
        leftCollision  = (x - height <= -fromIntegral width / 2) && (player >= y-(86/2) && player <= y+(86/2))
        rightCollision = (x + height >=  fromIntegral width / 2) && (player >= y-(86/2) && player <= y+(86/2))

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
    where
        height = 52

        (vx, vy) = ballVel game

        pl = if vx > 0 then player1 game else player2 game

        vx' = if paddleCollision (ballLoc game) (pl) height
            then -vx
            else vx

oppositeState :: KeyState -> KeyState
oppositeState Down = Up
oppositeState Up   = Down

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'r')           _    _ _) game = game { ballLoc = (0, 0) }
handleKeys (EventKey (Char 'p')           Down _ _) game | state == Running = game { gameState = Paused }
                                                         | otherwise        = game { gameState = Running }
                                                          where state = gameState game
handleKeys (EventKey (SpecialKey KeyUp)   _    _ _) game = game { player1Up = oppositeState (player1Up game), player1Do = Up                             }
handleKeys (EventKey (SpecialKey KeyDown) _    _ _) game = game { player1Up = Up                            , player1Do = oppositeState (player1Do game) }

handleKeys (EventKey (Char 'w')           _   _ _) game = game { player2Up = oppositeState (player2Up game), player2Do = Up                             }
handleKeys (EventKey (Char 's')           _    _ _) game = game { player2Up = Up                            , player2Do = oppositeState (player2Do game) }

-- Do nothing for all other events.
handleKeys _ game = game