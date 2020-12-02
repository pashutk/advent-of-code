{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "01"
, dependencies =
  [ "console"
  , "effect"
  , "lists"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "strings"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
