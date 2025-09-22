import Lake
open Lake DSL

package «lean-sdl-test» where
  version := v!"0.1.0"

require SDL3 from git "https://github.com/ValorZard/lean-sdl3.git" @ "master"

lean_lib LeanSdlTest where
  -- Add any library-specific configurations here

@[default_target]
lean_exe «lean-sdl-test» where
  root := `Main
  -- Link with SDL libraries using full paths to the import libraries
