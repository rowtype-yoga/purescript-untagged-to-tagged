{ name = "untagged-to-tagged"
, dependencies =
  [ "either"
  , "literals"
  , "newtype"
  , "prelude"
  , "record"
  , "type-equality"
  , "typelevel-lists"
  , "unsafe-coerce"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT-0"
, repository =
    "https://github.com/sigma-andex/purescript-untagged-to-tagged.git"
}
