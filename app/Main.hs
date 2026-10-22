{-# LANGUAGE InstanceSigs #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data GameState = GameState{
    elapsedTime::Float,
    player::[Player],
    bullets::[Bullet]
}
main :: IO ()
main =playIO
    (InWindow "Gloss Demo" (800, 600) (100, 100))
    white
    1
    GameState { elapsedTime = 0, player = [Player { playerPos = (0, 0), playerDir =(0,0) }], bullets = []}
    renderState
    input
    updateState

-- Input handling
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) _ _ _) gamestate = gamestate{bullets = Bullet{bulletPos=(getPosition $ head (player gamestate)), direction = (1, 0)}:(bullets gamestate)}
inputKey (EventKey (Char c) _ _ _) gameState = gameState
inputKey (EventKey (SpecialKey KeyUp) _ _ _) gamestate = gamestate{ player = map (\p -> p {playerDir = (0,1)}) (player gamestate)}
inputKey (EventKey (SpecialKey KeyDown) _ _ _) gamestate = gamestate{ player = map (\p -> p {playerDir = (0,-1)}) (player gamestate)}

inputKey _ gstate = gstate -- Otherwise keep the same

-- Update Loop
updateState:: Float -> GameState -> IO GameState
updateState dt gstate = return $ gstate { elapsedTime = elapsedTime gstate + dt, player = update (player gstate) dt, bullets = update (bullets gstate) dt }

--helper functions    
times :: Float -> Point -> Point
times x (a, b) = (a * x, b * x)
plus :: Point-> Point -> Point
plus (a, a2) (b, b2) = (a + b, a2 + b2)

-- Player
data Player = Player{
    playerPos::Point,
    playerDir::Vector
}
instance Movement Player where
    getPosition = playerPos
    setPosition a p = a {playerPos = p}
instance Renderable Player where
    render :: Player -> Picture
    render p = Translate (fst $ getPosition p) (snd $ getPosition p) (Circle 100)

instance Updateable Player where
    update :: [Player] -> Float -> [Player]
    update p dt = map (\x -> (move x (dt * 100) (playerDir x)){ playerDir = (0,0)}) p

-- Bullet
data Bullet = Bullet {
    bulletPos::Point,
    direction::Vector
}

instance Movement Bullet where
    getPosition = bulletPos
    setPosition a p = a {bulletPos = p}

instance Renderable Bullet where
    render :: Bullet -> Picture
    render p = Translate (fst $ getPosition p) (snd $ getPosition p) (Circle 20)

instance Updateable Bullet where
    update :: [Bullet] -> Float -> [Bullet]
    update b dt = map (\x -> move x dt (direction x)) b

-- Rendering
renderState :: GameState -> IO Picture
renderState x = return $ pictures $ [(renderWorld $ player x), renderWorld $ bullets x]

renderWorld:: Renderable a => [a] -> Picture
renderWorld x = pictures $ map render x

-- Classes
class Movement a where
    getPosition :: a -> Point
    setPosition :: a -> Point -> a
    move :: a -> Float -> Vector -> a
    move c dt dir = setPosition c $ plus pos $ times dt dir
        where
            pos = getPosition c

class Renderable a where
    render :: a -> Picture

class Updateable a where
    update :: [a]-> Float -> [a]