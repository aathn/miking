-- MLang AST symbolization.

include "mexpr/symbolize.mc"
include "mlang/ast.mc"

type DeclEnv = { synEnv : Map String Name,
                 conEnv : Map String Name,
                 tyEnv  : Map String Name,
                 semEnv : Map String Name }
con DeclEnv : DeclEnv -> SymDeclEnv

let declEnvEmpty =
  { synEnv = mapEmpty cmpString,
    conEnv = mapEmpty cmpString,
    tyEnv  = mapEmpty cmpString,
    semEnv = mapEmpty cmpString }

let mergeDecls : SymEnv -> DeclEnv -> SymEnv =
  lam e1. lam e2.
    {e1 with varEnv = mapUnion e1.varEnv e2.semEnv,
             conEnv = mapUnion e1.conEnv e2.conEnv,
             tyConEnv = mapUnion (mapUnion e1.tyConEnv e2.tyEnv) e2.synEnv}

let lookupLang : Map String (Name, SymDeclEnv) -> DeclEnv =
  use SymLookup in
  getSymbolWith
    { hasSym = lam. (t.ident, declEnvEmpty),
      absent = lam.
        if env.allowFree then (t.ident, declEnvEmpty)
        else
          symLookupError {
            kind = "language"
                     info = [t.info],
            allowFree = env.allowFree } t.ident,
      present = lam e. match e with (name, DeclEnv declEnv) in (name, declEnv)}

lang UseSym = Sym + UseAst
  sem symbolizeExpr env =
  | TmUse t ->
    match lookupLang t.ident env.langEnv with (ident, declEnv) in
    TmUse {t with ident = ident,
                  inexpr = symbolizeExpr (mergeDecls env declEnv) t.inexpr}
end


-- Symbolization for declarations
lang SymDecl = Sym + DeclAst
  sem symbolizeDecl : SymEnv -> Decl -> (SymEnv, Decl)

  sem setDeclSymbol : DeclEnv -> Decl -> (DeclEnv, Decl)
end

lang LangDeclSym = SymDecl + LangDeclAst
  sem symbolizeDecl (env : SymEnv) =
  | DeclLang t ->
    -- Collect names contained in this fragment
    match mapAccumL setDeclSymbol declEnvEmpty t.decls with (declEnv, decls) in

    -- Add names included from previous fragments
    let includedDeclEnvs = map lookupLang t.includes in
    match mapAccumL f declEnv includedDeclEnvs with (declEnv, includes) in

    -- Symbolize declarations in the fragment
    let symEnv = mergeDeclEnv env declEnv in
    match mapAccumL symbolizeDecl symEnv decls with (_ , decls) in

    -- Add language declaration mapping to symbolization environment
    match setSymbolWith (lam n. (n, declEnv)) t.ident symEnv.langEnv
      with (ident, langEnv) in

    ({symEnv with langEnv = langEnv},
     DeclLang {t with ident = ident,
                      includes = includes,
                      decls = decls})
end

lang SynDeclSym = SymDecl + SynDeclAst
  sem symbolizeDecl (env : SymDeclEnv) =
  -- {ident : Name,
  --  extends : [Name],
  --  defs : [{ident : Name, tyIdent : Type}],
  --  info : Info}
  | DeclSyn t ->
    match
      if nameHasSym t.ident then (env, t)
      else
        match env with SymDeclEnvDef (sde & {mexprEnv = mexprEnv, synEnv = synEnv}) in
        let ident = nameSetNewSym t.ident in
        let str = nameGetStr t.ident in
        let oldExtends = mapLookupOr [] str synEnv in
        let newExtends = cons ident oldExtends in
        let synEnv = mapInsert str newExtends synEnv in
        let t = {t with ident = ident, extends = oldExtends} in
        let tyConEnv = mapInsert str ident mexprEnv.tyConEnv in
        let mexprEnv = {mexprEnv with tyConEnv = tyConEnv} in
        (SymDeclEnvDef {sde with mexprEnv = mexprEnv, synEnv = synEnv}, t)
    with (env, t) in

    match
      mapAccumL (lam env: SymDeclEnv. lam condef.
        match env with SymDeclEnvDef (sde & {mexprEnv = mexprEnv}) in
        let tyIdent = symbolizeType mexprEnv condef.tyIdent in
        if nameHasSym condef.ident then
          (env, {condef with tyIdent = tyIdent})
        else
          let str = nameGetStr condef.ident in
          let ident = nameSetNewSym condef.ident in
          let conEnv = mapInsert str ident mexprEnv.conEnv in
          let mexprEnv = {mexprEnv with conEnv = conEnv} in
          let env = SymDeclEnvDef {sde with mexprEnv = mexprEnv} in
          (env, {condef with ident = ident, tyIdent = tyIdent})
      ) env t.defs
    with (env, defs) in
    (env, {t with defs = defs})
end

lang SemDeclSym = SymDecl + SemDeclAst
  sem symbolizeDecl (env : SymDeclEnv) =
  -- {ident : Name,
  --  extends : [Name],
  --  tyAnnot : Type,
  --  tyBody : Type,
  --  args : [{ident : Name, tyAnnot : Type}],
  --  cases : [{pat : Pat, thn : Expr}],
  --  info : Info}
  | DeclSem t ->
    match
      if nameHasSym t.ident then (env, t)
      else
        match env with SymDeclEnvDef (sde & {mexprEnv = mexprEnv, semEnv = semEnv}) in
        let ident = nameSetNewSym t.ident in
        let str = nameGetStr t.ident in
        let oldExtends = mapLookupOr [] str semEnv in
        let newExtends = cons ident oldExtends in
        let semEnv = mapInsert str newExtends semEnv in
        let t = {t with ident = ident, extends = oldExtends} in
        let varEnv = mapInsert str ident mexprEnv.varEnv in
        let mexprEnv = {mexprEnv with varEnv = varEnv} in
        (SymDeclEnvDef {sde with mexprEnv = mexprEnv, semEnv = semEnv}, t)
    with (env, t) in

    match env with SymDeclEnvDef {mexprEnv = mexprEnv} in
    let tyAnnot = symbolizeType mexprEnv t.tyAnnot in
    let tyBody = symbolizeType mexprEnv t.tyBody in
    let args = map (lam arg: {ident: Name, tyAnnot: Type}.
        let tyAnnot = symbolizeType mexprEnv arg.tyAnnot in
        if nameHasSym arg.ident then
            {ident = arg.ident, tyAnnot = tyAnnot}
        else
            {ident = nameSetNewSym arg.ident, tyAnnot = tyAnnot}
    ) t.args in
    let t = {t with tyAnnot = tyAnnot, tyBody = tyBody, args = args} in

    let casesEnv = foldl (lam casesEnv. lam arg.
      let str = nameGetStr arg.ident in
      let varEnv = mapInsert str arg.ident casesEnv.varEnv in
      {casesEnv with varEnv = varEnv}
    ) mexprEnv t.args in
    let cases = map (lam c: {pat: Pat, thn: Expr}.
      let pat = symbolizePat casesEnv (mapEmpty cmpString) c.pat in
      let thn = symbolizeExpr casesEnv thn in
      {pat = pat, thn = thn}
    ) t.cases in
    (env, DeclSem {t with cases = cases})
end
