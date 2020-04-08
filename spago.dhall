{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wai-websockets"
, dependencies = [ "console", "effect", "psci-support", "ws" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
