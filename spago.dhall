{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "generic-graphviz"
, dependencies =
    [ "console"
    , "dotlang"
    , "effect"
    , "generics-rep"
    , "graphviz"
    , "prelude"
    , "psci-support"
    , "test-unit"
    , "typelevel-prelude"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
