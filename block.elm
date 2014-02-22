import Signal
import Window

(block_width, block_height) = (100, 100)

speed = 1.5

dimensions = Window.dimensions
width = lift fst dimensions
height = lift snd dimensions

image = fittedImage block_width block_height "http://elm-lang.org/logo.png"

bound s width height = {s | vx <- if | s.x < 0 -> abs s.vx
                                     | (s.x + block_width) > width -> -(abs s.vx)
                                     | otherwise -> s.vx,
                          
                            vy <- if | s.y < 0 -> abs s.vy
                                     | (s.y + block_height) > height -> -(abs s.vy)
                                     | otherwise -> s.vy}

step dt state width height =
    let s = bound state width height in
        {s | x <- s.x + s.vx * dt, y <- s.y + s.vy * dt}

-- Ticks with the screen dimensions every millisecond
clock = sampleOn (every millisecond) dimensions

block = foldp (\(width, height) state -> step 1.0 state (toFloat width) (toFloat height))
    {x = 0, y = 0, vx = speed, vy = speed} clock

pos = lift (\s -> (s.x, s.y)) block

main = lift3 collage width height <|
         combine [lift2 move pos <|
          lift toForm <| lift4 container width height (constant bottomLeft) (constant image)]
