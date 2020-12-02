{ name = "lib"
, dependencies = [ "console", "effect", "psci-support", "prelude" ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
