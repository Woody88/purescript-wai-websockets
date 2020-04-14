{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wai-websockets"
, dependencies =
  [ "avar", "console", "effect", "psci-support", "wai", "ws" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
