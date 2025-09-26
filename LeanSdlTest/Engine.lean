import SDL

namespace Engine

structure Color where
  r : UInt8
  g : UInt8
  b : UInt8
  a : UInt8 := 255

structure EngineState where
  window : SDL.SDLWindow
  renderer : SDL.SDLRenderer
  deltaTime : Float
  lastTime : UInt32
  running : Bool
  playerX : Float
  playerY : Float
  playerSpeedY : Float := 0.0
  jumpStrength : Float := -300.0
  gravity : Float := 5.0
  terminalVelocity : Float := 500.0
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
  fillRect state.renderer state.playerX.toInt32 state.playerY.toInt32 100 100

  let _ ← SDL.renderTexture state.renderer state.texture 500 150 64 64

  let message := s!"Frames Per Second: {1 / state.deltaTime}s"
  let textSurface ← SDL.textToSurface state.renderer state.font message 50 50 255 255 255 255
  let textTexture ← SDL.createTextureFromSurface state.renderer textSurface
  let textWidth ← SDL.getTextureWidth textTexture
  let textHeight ← SDL.getTextureHeight textTexture
  let _ ← SDL.renderTexture state.renderer textTexture 50 50 textWidth textHeight
  pure ()

private def updateEngineState (engineState : IO.Ref EngineState) : IO Unit := do
  let state ← engineState.get
  let currentTime ← SDL.getTicks
  -- in seconds
  let deltaTime := (currentTime - state.lastTime).toFloat / 1000.0
  let mut speed := state.playerSpeedY + (state.gravity * deltaTime)
  let mut playerY := state.playerY

  if playerY > (SCREEN_HEIGHT - 100).toFloat then
    playerY := (SCREEN_HEIGHT - 100).toFloat
    speed := 0.0

  if ← isKeyDown .Space then speed := state.jumpStrength

  speed := speed + (state.gravity)

  playerY := playerY + (speed * deltaTime)

  engineState.set { state with deltaTime, lastTime := currentTime, playerY, playerSpeedY := speed }

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
    deltaTime := 0.0, lastTime := 0, running := true
    playerX := (SCREEN_WIDTH / 2).toFloat, playerY := (SCREEN_HEIGHT / 2).toFloat
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
