lang OCamlGenerate = OCamlTopAst
  sem generateTops : Expr -> Top

  sem generate : Expr -> Expr
  sem generate =
  | tm -> smap_Expr_Expr generate tm

  sem generateType : Type -> Type
  sem generateType =
  | ty -> smap_Type_Type generateType ty
end

lang OCamlGenerateLet = OCamlGenerate
  sem generateTops =
  | TmLet t ->
    cons
      (OTopLet { ident = t.ident
               , tyBody = generateType t.tyAnnot
               , body = generate t.body })
      (generateTops t.inexpr)
end

lang OCamlGenerateRecLets
  sem generateTops =
  | TmRecLets t ->
    let f = lam b.
      { ident = b.ident
      , tyBody = generateType b.tyAnnot
      , body = generate b.body
      } in
    cons
      (OTopRecLets { bindings = map f t.bindings })
      (generateTops t.inexpr)
end

lang OCamlGenerateType
  sem generateTops =
  | TmType t ->
    let decl =
      match t.tyIdent with TyVariant _ then
        OTopOpenVariantDecl {ident = t.ident, params = t.params}
      else
        OTopTypeDecl {ident = t.ident, params = t.params, ty = generateType t.tyIdent}
    in
    cons decl (generateTops t.inexpr)
end

lang OCamlGenerateCon
  sem generateTops =
  | TmConDef t ->
    match getConDefType t.tyIdent with TyCon {ident = ident} then
      OTopOpenVariantExt
        { ident = ident
        , constrs = mapFromSeq nameCmp [(t.ident, generateType t.tyIdent)]}
    else
      errorSingle [t.info]
        (join ["Invalid constructor type for constructor ", nameGetStr t.ident])
end

lang OCamlGenerateExt
  sem generateTops =
  | TmExt t ->
    match mapLookup t.ident env.exts with Some r then
      match convertData t.info env (OTmExprExt { expr = r.expr }) (r.ty, tyIdent)
      with (_, body) in
      cons
        (OTopLet { ident = t.ident, tyBody = generateType t.tyIdent, body = body })
        (generateTops t.inexpr)
    else
      errorSingle [t.info] (join ["No implementation for external ", nameGetStr ident])
end

