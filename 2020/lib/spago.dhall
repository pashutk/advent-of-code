{ name = "lib"
, dependencies =
  [ "catenable-lists"
  , "console"
  , "effect"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "string-parsers"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
