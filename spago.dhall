{ name = "untagged-to-tagged"
, dependencies =
  [ "either"
  , "newtype"
  , "prelude"
  , "record"
  , "typelevel-lists"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT-0"
, repository =
    "https://github.com/sigma-andex/purescript-untagged-to-tagged.git"
}
