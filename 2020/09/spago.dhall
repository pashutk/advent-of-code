{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "lib"
  , "numbers"
  , "partial"
  , "psci-support"
  , "string-parsers"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
