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
  -- negative is up in SDL
  jumpStrength : Float := -20.0
  gravity : Float := 1.0
  terminalVelocity : Float := 500.0
  wallSpeed : Float := 5.0
  walls : List AABBCollision := []
  wallSpawnTimer : UInt64 := 0
  wallSpawnInterval : UInt64 := 2000 -- in milliseconds
  isColliding : Bool := false
  texture : SDL.SDLTexture
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
  fillRect state.renderer state.player.x.toInt32 state.player.y.toInt32 state.player.width.toInt32 state.player.height.toInt32

  let _ ← SDL.renderTexture state.renderer state.texture 500 150 64 64

  for wall in state.walls do
    setColor state.renderer { r := 0, g := 255, b := 0 }
    fillRect state.renderer wall.x.toInt32 wall.y.toInt32 wall.width.toInt32 wall.height.toInt32

  let message := s!"Is Colliding: {state.isColliding}"
  let textSurface ← SDL.textToSurface state.renderer state.font message 50 50 255 255 255 255
  let textTexture ← SDL.createTextureFromSurface state.renderer textSurface
  let textWidth ← SDL.getTextureWidth textTexture
  let textHeight ← SDL.getTextureHeight textTexture
  let _ ← SDL.renderTexture state.renderer textTexture 50 50 textWidth textHeight
  pure ()

def physicsFramesPerSecond : Float := 60.0
def physicsDeltaTime : UInt64 := ((1.0 / physicsFramesPerSecond) * 1000.0).toUInt64 -- in milliseconds
def maxAccumulatedTime : UInt64 := 2500

private def physicsStep (state : EngineState) : IO EngineState := do
  let mut speed := state.playerSpeedY + state.gravity
  let mut playerY := state.player.y

  if playerY > (SCREEN_HEIGHT - 100).toFloat then
    playerY := (SCREEN_HEIGHT - 100).toFloat
    speed := 0.0

  if (← isKeyDown .Space) then speed := state.jumpStrength

  speed := speed + state.gravity

  playerY := playerY + speed

  -- add new walls
  let mut walls := state.walls
  let mut wallSpawnTimer := state.wallSpawnTimer + physicsDeltaTime
  if wallSpawnTimer >= state.wallSpawnInterval then
    let wallHeight : Int32 := 400
    walls := walls ++ [{ x := SCREEN_WIDTH.toFloat, y := (SCREEN_HEIGHT - wallHeight).toFloat, width := 100.0, height := wallHeight.toFloat }]
    wallSpawnTimer := 0

  -- move walls to the left
  walls := walls.map (fun wall => { wall with x := wall.x - state.wallSpeed })
  -- delete walls that are off screen
  walls := walls.filter (fun wall => wall.x + wall.width > -100)

  -- collision
  let mut isColliding := false
  for wall in walls do
    if aabbIntersects state.player wall then
      isColliding := true
      break

  return { state with player := { state.player with y := playerY }, playerSpeedY := speed, isColliding, walls, wallSpawnTimer }

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

  let texture ← try
    SDL.loadImageTexture renderer "assets/wall.png"
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
    player := { x := (SCREEN_WIDTH / 2).toFloat, y := 0, width := 100.0, height := 100.0 }
    texture := texture, font := font
  }

  let engineState ← IO.mkRef initialState
  IO.println "Starting game loop..."
  gameLoop engineState
  SDL.quit

def EngineState.setRunning (state : EngineState) (running : Bool) : EngineState :=
  { state with running }

def main : IO Unit :=
  run
