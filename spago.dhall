{ name = "puresf"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "foreign-object"
  , "maybe"
  , "prelude"
  , "profunctor"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
