{ name = "day1"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "node-fs"
  , "parsing"
  , "psci-support"
  , "stringutils"
  ]
, packages = ../../packages.dhall
, sources = [ "Main.purs" ]
}