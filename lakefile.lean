import Lake
open Lake DSL

package «lean-sdl-test» where
  version := v!"0.1.0"

require SDL3 from git "https://github.com/ValorZard/lean-sdl3.git" @ "master"

lean_lib LeanSdlTest where
  -- Add any library-specific configurations here

partial def copyAsset (src : System.FilePath) (dst : System.FilePath) : IO Unit := do
  for entry in (← src.readDir) do
    if entry.path.extension != none then
      copyFile entry.path (dst / entry.path.fileName.get!)
    else
      IO.FS.createDirAll (dst / entry.path.fileName.get!)
      copyAsset entry.path (dst / entry.path.fileName.get!)
  return ()

target copyAssets : Unit := Job.async do
  let dstDir := ((<- getRootPackage).binDir) / "assets"
  IO.FS.createDirAll dstDir
  let assetsDir : System.FilePath := (<- getRootPackage).dir / "assets"
  logInfo s!"Copying assets from {assetsDir} to {dstDir}"
  copyAsset assetsDir dstDir

@[default_target]
lean_exe «lean-sdl-test» where
  root := `Main
  needs := #[copyAssets]
  moreLinkArgs := if !System.Platform.isWindows then #["-Wl,--allow-shlib-undefined", "-Wl,-rpath=$ORIGIN"] else #[]
