{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "03t"
, dependencies =
  [ "console", "debug", "effect", "psci-support", "string-parsers" ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
