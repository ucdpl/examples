import Window

img = image 100 100 "http://elm-lang.org/logo.png"

wiggle x = lift (\n -> x * cos (n/500)) <| every millisecond
waggle x = lift (\n -> x * sin (n/500)) <| every millisecond

main = lift3 collage Window.width Window.height <| combine
  [lift2 moveY (waggle 300) <| lift2 moveX (wiggle 300) <| constant <| toForm img]
