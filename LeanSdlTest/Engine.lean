import SDL

namespace Engine

structure Color where
  r : UInt8
  g : UInt8
  b : UInt8
  a : UInt8 := 255

-- AABB Collision is really easy to write
structure AABBCollision where
  x : Float
  y : Float
  width : Float
  height : Float

def aabbIntersects (a b : AABBCollision) : Bool :=
  let collisionX := a.x + a.width >= b.x && b.x + b.width >= a.x
  let collisionY := a.y + a.height >= b.y && b.y + b.height >= a.y
  collisionX && collisionY

structure EngineState where
  window : SDL.SDLWindow
  renderer : SDL.SDLRenderer
  deltaTime : Float
  frameStart : UInt64 := 0
  accumulatedTime : UInt64 := 0
  running : Bool
  player : AABBCollision
  playerSpeedY : Float := 0.0
  playerTexture : SDL.SDLTexture
  -- negative is up in SDL
  jumpStrength : Float := -20.0
  gravity : Float := 1.0
  terminalVelocity : Float := 500.0
  wallSpeed : Float := 5.0
  topWalls : List AABBCollision := []
  bottomWalls : List AABBCollision := []
  wallTexture : SDL.SDLTexture
  wallSpawnTimer : UInt64 := 0
  wallSpawnInterval : UInt64 := 2000 -- in milliseconds
  isColliding : Bool := false
  score : Nat := 0
  highScore : Nat := 0
  font : SDL.SDLFont

def SCREEN_WIDTH : Int32 := 1280
def SCREEN_HEIGHT : Int32 := 720
def TEXTURE_SIZE : Float := 64.0

inductive Key where
  | W | A | S | D | Left | Right | Space | Escape

def keyToScancode : Key → UInt32
  | .W => SDL.SDL_SCANCODE_W | .A => SDL.SDL_SCANCODE_A | .S => SDL.SDL_SCANCODE_S
  | .D => SDL.SDL_SCANCODE_D | .Left => SDL.SDL_SCANCODE_LEFT | .Right => SDL.SDL_SCANCODE_RIGHT
  | .Space => SDL.SDL_SCANCODE_SPACE | .Escape => SDL.SDL_SCANCODE_ESCAPE

def isKeyDown (key : Key) : IO Bool := SDL.getKeyState (keyToScancode key)

def setColor (renderer : SDL.SDLRenderer) (color : Color) : IO Unit :=
  SDL.setRenderDrawColor renderer color.r color.g color.b color.a *> pure ()

def fillRect (renderer : SDL.SDLRenderer) (x y w h : Int32) : IO Unit :=
  SDL.renderFillRect renderer x y w h *> pure ()

def renderScene (state : EngineState) : IO Unit := do
  setColor state.renderer { r := 135, g := 206, b := 235 }
  let _ ← SDL.renderClear state.renderer

  setColor state.renderer { r := 255, g := 0, b := 0 }
  let _ <- SDL.renderTexture state.renderer state.playerTexture 0 0 16 16 state.player.x.toInt64 state.player.y.toInt64 state.player.width.toInt64 state.player.height.toInt64

  for wall in state.bottomWalls do
    setColor state.renderer { r := 0, g := 255, b := 0 }
    let _ <- SDL.renderTexture state.renderer state.wallTexture 0 0 32 40 wall.x.toInt64 wall.y.toInt64 wall.width.toInt64 wall.height.toInt64

  for wall in state.topWalls do
    setColor state.renderer { r := 0, g := 255, b := 0 }
    let _ <- SDL.renderTexture state.renderer state.wallTexture 0 40 32 40 wall.x.toInt64 wall.y.toInt64 wall.width.toInt64 wall.height.toInt64


  let message := s!"Score: {state.score} High Score: {state.highScore}"
  let textSurface ← SDL.textToSurface state.renderer state.font message 50 50 255 255 255 255
  let textTexture ← SDL.createTextureFromSurface state.renderer textSurface
  let textWidth ← SDL.getTextureWidth textTexture
  let textHeight ← SDL.getTextureHeight textTexture
  let _ ← SDL.renderEntireTexture state.renderer textTexture 50 50 textWidth textHeight
  pure ()

def physicsFramesPerSecond : Float := 60.0
def physicsDeltaTime : UInt64 := ((1.0 / physicsFramesPerSecond) * 1000.0).toUInt64 -- in milliseconds
def maxAccumulatedTime : UInt64 := 2500

private def physicsStep (state : EngineState) : IO EngineState := do
  let mut speed := state.playerSpeedY + state.gravity
  let mut player := state.player

  -- collision with ground
  if player.y > SCREEN_HEIGHT.toFloat - player.height then
    player := { player with y := SCREEN_HEIGHT.toFloat - player.height }
    speed := 0.0

  let mut score := state.score
  -- add new walls
  let mut bottomWalls := state.bottomWalls
  let mut topWalls := state.topWalls
   -- collision
  let mut isColliding := false
  for wall in bottomWalls ++ topWalls do
    if aabbIntersects player wall then
      isColliding := true
      break

  -- collision with ceiling
  if player.y < 0.0 then
    player := { player with y := 0.0 }
    isColliding := true

  -- reset score on collision
  let mut highScore := state.highScore
  if isColliding then
    if score > highScore then
      highScore := score
    score := 0

  -- game logic
  if (← isKeyDown .Space) then speed := state.jumpStrength

  speed := speed + state.gravity

  player := { player with y := player.y + speed }

  -- add new walls
  let mut wallSpawnTimer := state.wallSpawnTimer + physicsDeltaTime
  if wallSpawnTimer >= state.wallSpawnInterval then
    let wallHeight : Int32 := (← IO.rand 100 400).toInt32
    let gapHeight  : Int32 := (← IO.rand 200 300).toInt32
    topWalls := topWalls ++ [
      -- top wall
      { x := SCREEN_WIDTH.toFloat, y := 0.0, width := 100.0, height := (SCREEN_HEIGHT - (wallHeight + gapHeight)).toFloat },
    ]
    bottomWalls := bottomWalls ++ [
      { x := SCREEN_WIDTH.toFloat, y := (SCREEN_HEIGHT - wallHeight).toFloat, width := 100.0, height := wallHeight.toFloat }
    ]
    wallSpawnTimer := 0

  -- move walls to the left
  bottomWalls := bottomWalls.map (fun wall => { wall with x := wall.x - state.wallSpeed })
  topWalls := topWalls.map (fun wall => { wall with x := wall.x - state.wallSpeed })
  -- delete walls that are off screen
  let newBottomWalls := bottomWalls.filter (fun wall => wall.x + wall.width > -100)
  let newTopWalls := topWalls.filter (fun wall => wall.x + wall.width > -100)

  -- only count score of bottom walls to avoid double counting
  score := score + (bottomWalls.length - newBottomWalls.length)

  bottomWalls := newBottomWalls
  topWalls := newTopWalls

  return { state with player, playerSpeedY := speed, isColliding, bottomWalls, topWalls, wallSpawnTimer, score, highScore }

-- using this article for the fixed time step
-- https://code.tutsplus.com/how-to-create-a-custom-2d-physics-engine-the-core-engine--gamedev-7493t
private def updateEngineState (engineState : IO.Ref EngineState) : IO Unit := do
  let mut state ← engineState.get
  let currentTime ← SDL.getTicks
  -- get the time elapsed since last frame in milliseconds
  let mut accumulatedTime := state.accumulatedTime + (currentTime - state.frameStart)
  -- update the frame start time to now
  let frameStart := currentTime

  -- if we somehow accumulated too much time, clamp it
  if accumulatedTime > maxAccumulatedTime then
    accumulatedTime := maxAccumulatedTime

  while (accumulatedTime >= physicsDeltaTime) do
    state ← physicsStep state
    accumulatedTime := accumulatedTime - physicsDeltaTime

  engineState.set { state with frameStart, accumulatedTime }

partial def gameLoop (engineState : IO.Ref EngineState) : IO Unit := do
  updateEngineState engineState

  let eventType ← SDL.pollEvent
  if eventType == SDL.SDL_QUIT || (← isKeyDown .Escape) then
    engineState.modify (fun s => { s with running := false })

  let state ← engineState.get
  if state.running then
    renderScene state
    SDL.renderPresent state.renderer
    gameLoop engineState

partial def run : IO Unit := do
  unless (← SDL.init SDL.SDL_INIT_VIDEO) == 1 do
    IO.println "Failed to initialize SDL"
    return

  let window ← try
    SDL.createWindow "LeanDoomed" SCREEN_WIDTH SCREEN_HEIGHT SDL.SDL_WINDOW_SHOWN
  catch sdlError =>
    IO.println sdlError
    SDL.quit
    return

  let renderer ← try
    SDL.createRenderer window
  catch sdlError =>
    IO.println sdlError
    SDL.quit
    return

  let playerTexture ← try
    SDL.loadImageTexture renderer "assets/FlappyBirdAssets/Player/StyleBird1/Bird1-1.png"
  catch sdlError =>
    IO.println sdlError
    SDL.quit
    return

  let wallTexture ← try
    SDL.loadImageTexture renderer "assets/FlappyBirdAssets/Tiles/Style 1/PipeStyle1.png"
  catch sdlError =>
    IO.println sdlError
    SDL.quit
    return

  unless (← SDL.ttfInit) do
    IO.println "Failed to initialize SDL_ttf"
    SDL.quit
    return

  let font ← try
    SDL.loadFont "assets/Inter-VariableFont.ttf" 24
  catch sdlError =>
    IO.println sdlError
    SDL.quit
    return

  unless (← SDL.mixerInit) do
    IO.println "Failed to initialize SDL_mixer"
    SDL.quit
    return

  let initialState : EngineState := {
    window := window, renderer := renderer
    deltaTime := 0.0, frameStart := 0, running := true
    player := { x := 100.0, y := 0, width := 50.0, height := 50.0 }
    playerTexture, font, wallTexture
  }

  let engineState ← IO.mkRef initialState
  IO.println "Starting game loop..."
  gameLoop engineState
  SDL.quit

def EngineState.setRunning (state : EngineState) (running : Bool) : EngineState :=
  { state with running }

def main : IO Unit :=
  run
