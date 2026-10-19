{-# LANGUAGE InstanceSigs #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data GameState = GameState{
    elapsedTime::Float,
    player::Player
}
main :: IO ()
main =playIO
    (InWindow "Gloss Demo" (800, 600) (100, 100))
    white
    1
    GameState { elapsedTime = 0, player = Player { playerPos = (0, 0) }}
    renderState
    input
    update

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gameState = gameState
inputKey (EventKey (SpecialKey KeyUp) _ _ _) gamestate = gamestate{ player = move (player gamestate) 1.0 (0,100)}
inputKey (EventKey (SpecialKey KeyDown) _ _ _) gamestate = gamestate{ player = move (player gamestate) 1.0 (0,-100)}

inputKey _ gstate = gstate -- Otherwise keep the same

update :: Float -> GameState -> IO GameState
update dt gstate = return $ gstate { elapsedTime = elapsedTime gstate + dt }

data Player = Player{
    playerPos::Point
}
    
times :: Float -> Point -> Point
times x (a, b) = (a * x, b * x)
plus :: Point-> Point -> Point
plus (a, a2) (b, b2) = (a + b, a2 + b2)

class Movement a where
    getPosition :: a -> Point
    setPosition :: a -> Point -> a
    move :: a -> Float -> Vector -> a
    move c dt dir = setPosition c $ plus pos $ times dt dir
        where
            pos = getPosition c

instance Movement Player where
    getPosition = playerPos
    setPosition a p = a {playerPos = p}
instance Renderable Player where
    render :: Player -> Picture
    render p = Translate (fst $ playerPos p) (snd $ playerPos p) (Circle 100)

renderState :: GameState -> IO Picture
renderState x = return $ renderWorld $ [player x]

renderWorld:: Renderable a => [a] -> Picture
renderWorld x = pictures $ map render x

class Renderable a where
    render :: a -> Picture