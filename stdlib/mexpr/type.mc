include "map.mc"
include "mexpr/ast.mc"

-- Unwraps type alias `ty` from `aliases`.
recursive let typeUnwrapAlias = use MExprAst in
  lam aliases : Map Name Type. lam ty : Type.
  match ty with TyCon {ident = ident} then
    match mapLookup ident aliases with Some ty then
      typeUnwrapAlias aliases ty
    else ty
  else ty
end

let builtinTypes : [(String, [String])] =
  [ ("Symbol", [])
  , ("Ref", ["a"])
  , ("Map", ["k", "v"])
  , ("BootParseTree", [])
  ]
