{ name = "lib"
, dependencies =
  [ "console", "effect", "prelude", "psci-support", "string-parsers" ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
