-- Unification of MExpr types.  See type-check.mc for an overview of
-- the type checker.

include "result.mc"

include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/eq.mc"
include "mexpr/info.mc"
include "mexpr/type.mc"


----------------------
-- TYPE UNIFICATION --
----------------------

lang Unify = Ast
  type UnifyEnv = {
    wrappedLhs: Type,  -- The currently examined left-hand subtype, before resolving aliases
    wrappedRhs: Type,  -- The currently examined right-hand subtype, before resolving aliases
    boundNames: BiNameMap  -- The bijective correspondence between bound variables in scope
  }

  syn UnifyError =
  | Types (Type, Type)
  | Records (Map SID Type, Map SID Type)
  | Kinds (Kind, Kind)

  type Unifier u = {
    empty : u,
    combine : u -> u -> u,
    unify : UnifyEnv -> Type -> Type -> u,
    err   : UnifyError -> u
  }

  -- Unify the types `ty1` and `ty2` under the assumptions of `env`.
  -- Takes an abstract type `u` satisfying the interface `Unifier` to
  -- perform the actual variable unifications.
  -- For an example usage, see `unifyPure` below.
  sem unifyTypes : all u. Unifier u -> UnifyEnv -> (Type, Type) -> u
  sem unifyTypes u env =
  | (ty1, ty2) ->
    unifyBase u
      {env with wrappedLhs = ty1, wrappedRhs = ty2}
      (unwrapType ty1, unwrapType ty2)

  -- unifyBase env (ty1, ty2) unifies ty1 and ty2 under the
  -- assumptions of env.
  -- IMPORTANT: Assumes that ty1 = unwrapType env.wrappedLhs and
  -- ty2 = unwrapType env.wrappedRhs.
  sem unifyBase : all u. Unifier u -> UnifyEnv -> (Type, Type) -> u
  sem unifyBase u env =
  | (ty1, ty2) ->
    u.err (Types (ty1, ty2))

  sem unifyKinds : all u. Unifier u -> UnifyEnv -> (Kind, Kind) -> u
  sem unifyKinds u env =
  | (k1, k2) -> u.err (Kinds (k1, k2))

  sem addKinds : all u. Unifier u -> UnifyEnv -> (Kind, Kind) -> (u, Kind)
  sem addKinds u env =
  | (k1, k2) -> (u.err (Kinds (k1, k2)), k1)
end

-- Helper language providing functions to unify fields of record-like types
lang UnifyRecords = Unify
  -- Check that 'm1' is a subset of 'm2'
  sem unifyRecordsSubset : all u. Unifier u -> UnifyEnv -> Map SID Type -> Map SID Type -> u
  sem unifyRecordsSubset u env m1 =
  | m2 ->
    let f = lam acc. lam b.
      let unifier =
        match b with (k, tyfield1) in
        match mapLookup k m2 with Some tyfield2 then
          unifyTypes u env (tyfield1, tyfield2)
        else
          u.err (Records (m1, m2))
      in
      u.combine acc unifier
    in
    foldl f u.empty (mapBindings m1)

  -- Check that 'm1' and 'm2' contain the same fields
  sem unifyRecordsStrict : all u. Unifier u -> UnifyEnv -> Map SID Type -> Map SID Type -> u
  sem unifyRecordsStrict u env m1 =
  | m2 ->
    if eqi (mapSize m1) (mapSize m2) then
      unifyRecordsSubset u env m1 m2
    else
      u.err (Records (m1, m2))

  -- Check that the intersection of 'm1' and 'm2' unifies, then return their union
  sem unifyRecordsUnion : all u. Unifier u -> UnifyEnv -> Map SID Type -> Map SID Type -> (u, Map SID Type)
  sem unifyRecordsUnion u env m1 =
  | m2 ->
    let f = lam acc. lam b.
      match b with (k, tyfield1) in
      match mapLookup k acc.1 with Some tyfield2 then
        (u.combine acc.0 (unifyTypes u env (tyfield1, tyfield2)), acc.1)
      else
        (acc.0, mapInsert k tyfield1 acc.1)
    in
    foldl f (u.empty, m2) (mapBindings m1)
end

lang VarTypeUnify = Unify + VarTypeAst
  sem unifyBase u env =
  | (TyVar t1 & ty1, TyVar t2 & ty2) ->
    if nameEq t1.ident t2.ident then u.empty
    else if biMem (t1.ident, t2.ident) env.boundNames then u.empty
    else u.err (Types (ty1, ty2))
end

lang MetaVarTypeUnify = Unify + MetaVarTypeAst
  sem unifyBase u env =
  | (TyMetaVar _ & ty1, ty2) -> u.unify env ty1 ty2
  | (!TyMetaVar _ & ty1, TyMetaVar _ & ty2) ->
    unifyBase u {env with wrappedLhs = env.wrappedRhs, wrappedRhs = env.wrappedLhs} (ty2, ty1)
end

lang FunTypeUnify = Unify + FunTypeAst
  sem unifyBase u env =
  | (TyArrow t1, TyArrow t2) ->
    u.combine
      (unifyTypes u env (t1.from, t2.from))
      (unifyTypes u env (t1.to, t2.to))
end

lang AppTypeUnify = Unify + AppTypeAst
  sem unifyBase u env =
  | (TyApp t1, TyApp t2) ->
    u.combine
      (unifyTypes u env (t1.lhs, t2.lhs))
      (unifyTypes u env (t1.rhs, t2.rhs))
end

lang AllTypeUnify = Unify + AllTypeAst
  sem unifyBase u env =
  | (TyAll t1, TyAll t2) ->
    u.combine
      (unifyKinds u env (t1.kind, t2.kind))
      (let env = {env with boundNames = biInsert (t1.ident, t2.ident) env.boundNames} in
       unifyTypes u env (t1.ty, t2.ty))
end

lang ConTypeUnify = Unify + ConTypeAst
  sem unifyBase u env =
  | (TyCon t1 & ty1, TyCon t2 & ty2) ->
    if nameEq t1.ident t2.ident then
      unifyTypes u env (t1.data, t2.data)
    else
      u.err (Types (ty1, ty2))
end

lang DataTypeUnify = Unify + DataTypeAst
  sem unifyBase u env =
  | (TyData t1 & ty1, TyData t2 & ty2) ->
    if mapEq setEq (computeData t1) (computeData t2) then u.empty
    else
      u.err (Types (ty1, ty2))
end

lang BoolTypeUnify = Unify + BoolTypeAst
  sem unifyBase u env =
  | (TyBool _, TyBool _) -> u.empty
end

lang IntTypeUnify = Unify + IntTypeAst
  sem unifyBase u env =
  | (TyInt _, TyInt _) -> u.empty
end

lang FloatTypeUnify = Unify + FloatTypeAst
  sem unifyBase u env =
  | (TyFloat _, TyFloat _) -> u.empty
end

lang CharTypeUnify = Unify + CharTypeAst
  sem unifyBase u env =
  | (TyChar _, TyChar _) -> u.empty
end

lang SeqTypeUnify = Unify + SeqTypeAst
  sem unifyBase u env =
  | (TySeq t1, TySeq t2) ->
    unifyTypes u env (t1.ty, t2.ty)
end

lang TensorTypeUnify = Unify + TensorTypeAst
  sem unifyBase u env =
  | (TyTensor t1, TyTensor t2) ->
    unifyTypes u env (t1.ty, t2.ty)
end

lang RecordTypeUnify = UnifyRecords + RecordTypeAst
  sem unifyBase u env =
  | (TyRecord t1, TyRecord t2) ->
    unifyRecordsStrict u env t1.fields t2.fields
end

lang BaseKindUnify = Unify + PolyKindAst + MonoKindAst
  sem unifyKinds u env =
  | (_, Mono () | Poly ()) -> u.empty

  sem addKinds u env =
  | (Mono _ | Poly _, !(Mono _ | Poly _) & k)
  | (!(Mono _ | Poly _) & k, Mono _ | Poly _)
  | (Poly _, (Poly _ | Mono _) & k) ->
    (u.empty, k)
  | (Mono _, Poly _ | Mono _) ->
    (u.empty, Mono ())
end

lang RecordKindUnify = UnifyRecords + RecordKindAst
  sem unifyKinds u env =
  | (Record r1, Record r2) ->
    unifyRecordsSubset u env r2.fields r1.fields

  sem addKinds u env =
  | (Record r1, Record r2) ->
    match unifyRecordsUnion u env r1.fields r2.fields with (unifier, fields) in
    (unifier, Record {r1 with fields = fields})
end

lang DataKindUnify = Unify + DataKindAst
  sem unifyKinds u env =
  | (Data r1, Data r2) ->
    if mapAllWithKey
         (lam t. lam ks2.
           optionMapOr false
             (lam ks1.
               if ks2.covariant
               then and ks1.covariant (setSubset ks1.cons ks2.cons)
               else and (not ks1.covariant) (setSubset ks2.cons ks1.cons))
             (mapLookup t r1.types))
         r2.types
    then u.empty
    else u.err (Kinds (Data r1, Data r2))

  sem addKinds u env =
  | (Data r1, Data r2) ->
    match
      mapMapAccum
        (lam acc. lam t. lam ks1.
          match mapLookup t r2.types with Some ks2 then
            if xor ks1.covariant ks2.covariant
            then (u.err (Kinds (Data r1, Data r2)), ks1)
            else (acc, {ks1 with cons = setUnion ks1.cons ks2.cons})
          else (acc, ks1))
        u.empty r1.types
    with (acc, types)
    in (acc, Data {r1 with types = types})
end

lang UnifyPure = Unify + MetaVarTypeAst + VarTypeSubstitute

  type UnifyPureResult a = Result () UnifyError a
  type UnifyPureUnifier = [(UnifyEnv, Type, Type)]

  -- Unify types `ty1` and `ty2`, returning a map of variable substitutions
  -- equating the two, or giving an error if the types are incompatible.
  -- This function does not perform any occurs checks, scope checking, or
  -- level updates, and accepts cyclic equations.
  sem unifyPure : Type -> Type -> UnifyPureResult (Map Name Type)
  sem unifyPure ty1 = | ty2 ->
    let u : Unifier (UnifyPureResult UnifyPureUnifier) = {
      empty = result.ok [],
      combine = result.map2 concat,
      unify = lam env. lam ty1. lam ty2. result.ok [(env, ty1, ty2)],
      err = result.err
    }
    in
    recursive let work = lam acc. lam unifier.
      switch unifier
      case [] then result.ok acc
      case [ (env, meta, ty) ] ++ rest then
        switch unwrapType meta
        case TyMetaVar t then
          match deref t.contents with Unbound r in
          let isEqual =
            match unwrapType ty with TyMetaVar t2 then
              match deref t2.contents with Unbound r2 in nameEq r.ident r2.ident
            else false
          in
          if isEqual then work acc rest else
            if mapMem r.ident acc then work acc rest else
              let subst = mapInsert r.ident ty (mapEmpty nameCmp) in
              let f = substituteMetaVars subst in
              let g = lam x. (x.0, f x.1, f x.2) in
              work (mapUnion (mapMap f acc) subst) (map g rest)
        case other then
          result.bind (unifyTypes u env (other, ty))
            (lam newUnifier. work acc (concat newUnifier rest))
        end
      end
    in
    let env : UnifyEnv = {
      boundNames = biEmpty,
      wrappedLhs = ty1,
      wrappedRhs = ty2
    } in
    result.bind (unifyTypes u env (ty1, ty2)) (work (mapEmpty nameCmp))
end


lang MExprUnify =
  VarTypeUnify + MetaVarTypeUnify + FunTypeUnify + AppTypeUnify + AllTypeUnify +
  ConTypeUnify + DataTypeUnify + BoolTypeUnify + IntTypeUnify + FloatTypeUnify +
  CharTypeUnify + SeqTypeUnify + TensorTypeUnify + RecordTypeUnify +

  BaseKindUnify + RecordKindUnify + DataKindUnify
end

lang TestLang = UnifyPure + MExprUnify + MExprEq + MetaVarTypeEq end

mexpr

use TestLang in

let eqUnifyError = lam e1. lam e2.
  switch (e1, e2)
  case (Types t1, Types t2) then and (eqType t1.0 t2.0) (eqType t1.1 t2.1)
  case _ then error "eqUnifyError: TODO"
  end
in

let unifyEq = eitherEq (eqSeq eqUnifyError) (mapEq eqType) in

let a = nameSym "a" in
let b = nameSym "b" in

let wa =
  TyMetaVar {info = NoInfo (),
             contents = ref (Unbound {ident = a,
                                      level = 0,
                                      kind  = Mono ()})} in
let wb =
  TyMetaVar {info = NoInfo (),
             contents = ref (Unbound {ident = b,
                                      level = 0,
                                      kind  = Mono ()})} in

let ok  = lam x. Right (mapFromSeq nameCmp x) in
let err = lam x. Left (map (lam y. Types y) x) in
let testUnify = lam ty1. lam ty2. (result.consume (unifyPure ty1 ty2)).1 in

utest testUnify tyint_ tyint_ with ok [] using unifyEq in

utest testUnify tybool_ tyint_ with err [(tybool_, tyint_)] using unifyEq in

utest testUnify wa tyint_ with ok [(a, tyint_)] using unifyEq in

utest testUnify (tyarrow_ wa wb) (tyarrow_ tyint_ tybool_)
  with ok [(a, tyint_), (b, tybool_)]
  using unifyEq
in

utest testUnify (tyarrow_ wa wa) (tyarrow_ tyint_ tybool_)
  with err [(tyint_, tybool_)]
  using unifyEq
in

utest testUnify (tyarrow_ wa tybool_) (tyarrow_ wb wb)
  with ok [(a, tybool_), (b, tybool_)]
  using unifyEq
in

utest testUnify (tytuple_ [wa, wb]) (tytuple_ [wa, wa])
  with ok [(b, wa)]
  using unifyEq
in

utest testUnify (tytuple_ [wa, wa]) (tytuple_ [tyseq_ wa, tyseq_ (tyseq_ wa)])
  with ok [(a, tyseq_ wa)]
  using unifyEq
in

()
