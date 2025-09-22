import Lake
open Lake DSL

package «lean-sdl-test» where
  version := v!"0.1.0"

require SDL3 from git "https://github.com/ValorZard/lean-sdl3.git" @ "master"

@[default_target]
lean_lib LeanSdlTest where
  -- Add any library-specific configurations here

lean_exe «lean-sdl-test» where
  root := `Main
  -- Link with SDL libraries using full paths to the import libraries
  moreLinkArgs := #[
    s!"-L{__dir__ / ".lake" / "packages" / "SDL3" / "vendor" / "SDL" / "build"}",
    s!"-L{__dir__ / ".lake" / "packages" / "SDL3" / "vendor" / "SDL_image" / "build"}",
  ]
