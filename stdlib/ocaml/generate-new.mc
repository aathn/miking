include "option.mc"
include "effect.mc"

include "ocaml/ast.mc"
include "ocaml/generate-env.mc"
include "ocaml/external.mc"

include "mexpr/ast-builder.mc"
include "mexpr/ast-effect.mc"
include "mexpr/type.mc"

lang OCamlGenerate = OCamlTopAst + AstEffect
  sem generateTops : Expr -> Eff [Top]
  sem generateTops =
  | tm ->
    effMap (lam x. [OTopExpr {expr = x}]) (generate tm)

  sem generate : Expr -> Eff Expr
  sem generate =
  | tm -> smapEff_Expr_Expr generate tm

  sem generateType : Type -> Eff Type
  sem generateType =
  | ty -> smapEff_Type_Type generateType ty
end

lang OCamlGenerateLet = OCamlGenerate + LetAst
  sem generateTops =
  | TmLet t ->
    effMap2 cons
      (effMap2
         (lam tyBody. lam body.
         (OTopLet { ident = t.ident
                  , tyBody = tyBody
                  , body = body }))
         (generateType t.tyAnnot)
         (generate t.body))
      (generateTops t.inexpr)
end

lang OCamlGenerateRecLets = OCamlGenerate + RecLetsAst
  sem generateTops =
  | TmRecLets t ->
    let f =
      lam b.
      effMap2
        (lam tyBody. lam body.
        { ident = b.ident
        , tyBody = tyBody
        , body = body
        })
        (generateType b.tyAnnot)
        (generate b.body)
    in
    effMap2
      (lam bindings. lam rest.
      cons (OTopRecLets { bindings = bindings }) rest)
      (effMapM f t.bindings)
      (generateTops t.inexpr)
end

lang OCamlGenerateType = OCamlGenerate + TypeAst + VariantTypeAst
  sem generateTops =
  | TmType t ->
    let decl =
      match t.tyIdent with TyVariant _ then
        return (OTopOpenVariantDecl {ident = t.ident, params = t.params})
      else
        effMap
          (lam ty. OTopTypeDecl {ident = t.ident, params = t.params, ty = ty})
          (generateType t.tyIdent)
    in
    effMap2 cons decl (generateTops t.inexpr)
end

lang OCamlGenerateCon = OCamlGenerate + DataAst + ConTypeAst + ConDefTypeUtils + Failure
  sem invalidConstructorFailure : Info -> Name -> Failure

  sem generateTops =
  | TmConDef t ->
    match getConDefType t.tyIdent with TyCon {ident = ident} then
      effMap2
        (lam tyIdent. lam rest.
        cons
          (OTopOpenVariantExt
             { ident = ident
             , constrs = mapFromSeq nameCmp [(t.ident, tyIdent)] })
          rest)
        (generateType t.tyIdent)
        (generateTops t.inexpr)
    else
      fail (invalidConstructorFailure t.info t.ident)
end

lang OCamlGenerateExt =
  OCamlGenerate + ExtAst + OCamlExternal + OCamlGenerateExternalNaive +
  Reader + Failure

  sem missingExternalFailure : Info -> Name -> Failure
  sem getGenerateEnv : Ctx -> GenerateEnv

  sem generateTops =
  | TmExt t ->
    bind (ask getGenerateEnv) (lam env.
    match mapLookup t.ident env.exts with Some ([r] ++ _) then
      match convertData t.info env (OTmExprExt { expr = r.expr }) (r.ty, t.tyIdent)
      with (_, body) in
      effMap2
        (lam tyBody. lam rest.
        cons
          (OTopLet { ident = t.ident, tyBody = tyBody, body = body }) rest)
        (generateType t.tyIdent)
        (generateTops t.inexpr)
    else
      fail (missingExternalFailure t.info t.ident))
end

lang OCamlGenerateMExpr =
  OCamlGenerateLet + OCamlGenerateRecLets + OCamlGenerateType +
  OCamlGenerateCon + OCamlGenerateExt
end

mexpr

use OCamlGenerateMExpr in

(lam x. lam y. x) 0 (generateTops (var_ "hello"))
