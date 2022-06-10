open Syntax

exception Error of string

(* 例外を発生させるための関数 *)
let err s = raise (Error s)

(* トークンと型のタプルのリスト。変数がどの型に束縛されているかが入っている *)
type tyenv = ty Environment.t

(* 型代入を表す型 *)
type subst = (tyvar * ty) list

(* 型代入を適用する関数 *)
let rec subst_type subst ty =
  match subst with
  (* subst型の値lの最初の型代入を抜き出してassign_typeを行います。 *)
  | (tv, t) :: rest ->
      let rec assign_type ty tv t =
        match ty with
        | TyInt -> TyInt
        | TyBool -> TyBool
        (* 型代入が適用できるなら適用します。 *)
        | TyVar a -> if a = tv then t else TyVar a
        (* 型代入θに対して、θ(τ1 -> τ2) = θτ1 -> θτ2を行います。 *)
        | TyFun (x, y) -> TyFun (assign_type x tv t, assign_type y tv t)
        | TyList x -> TyList (assign_type x tv t)
      in
      subst_type rest (assign_type ty tv t)
  (* 最後までassign_typeを適用し終えると、適用後のtyを返します。 *)
  | [] -> ty

(* 型の等式集合に型代入を適用する関数 *)
let rec subst_eqs s eqs =
  match s with
  (* 型代入を一つ取り出します。 *)
  | tv, t -> (
      match eqs with
      (* 等式集合を一つ取り出してそれに取り出した型代入を適用させます。これを再帰的に繰り返します。 *)
      | (x, y) :: rest ->
          if x = tv && y = tv then (t, t) :: subst_eqs s rest
          else if x = tv then (t, y) :: subst_eqs s rest
          else if y = tv then (x, t) :: subst_eqs s rest
          else (x, y) :: subst_eqs s rest
      | [] -> [])

(* 型代入を型の等式集合に変換する関数 *)
let rec eqs_of_subst s =
  match s with
  (* tyvar型の値にTyVarを付けることによってty型と認識させ、tyvar * tyのリストをty * tyのリストに変換しています。 *)
  | (tv, t) :: rest -> ( match tv with a -> (TyVar a, t) :: eqs_of_subst rest)
  | [] -> []

(* 型代入をリストに対して行う関数 *)
let rec subst_type_list l s =
  match s with
  (* subst_typeの第二引数もリストになったバージョンです。第二引数から一つずつ組を取り出してsubst_typeを行います。 *)
  | (x, y) :: rest ->
      (subst_type [ l ] x, subst_type [ l ] y) :: subst_type_list l rest
  | [] -> []

(* 型の単一化を行う関数 *)
let rec unify v =
  match v with
  (* 引数のリストの最初の組を取り出します。 *)
  | (l, r) :: rest -> (
      if l = r then unify rest
      else
        match (l, r) with
        (* 等式制約が共にTyFunの時はその第一引数と第二引数同士が等しいという等式制約に変換します。 *)
        | TyFun (x, y), TyFun (s, t) ->
            unify ((x, s) :: (y, t) :: rest)
            (* 等式制約の片方の辺が型変数の時、オカーチェックが通れば型変数を全てもう一辺に変換してまたその等式制約をUnifyの外に出します *)
        | TyVar l, r ->
            if MySet.member l (freevar_ty r) then err "Cannot Unification!"
            else (l, r) :: unify (subst_type_list (l, r) rest)
        | l, TyVar r ->
            if MySet.member r (freevar_ty l) then err "Cannot Unification!"
            else (r, l) :: unify (subst_type_list (r, l) rest)
        | TyList l, TyList r -> unify ((l, r) :: rest)
        | _ -> err "Cannot Unification!")
  | [] -> []

(* 二項演算子から引数の型と返り値の型が何になるべきかを求める関数 *)
let ty_prim op ty1 ty2 =
  match op with
  | Plus ->
      ([ (ty1, TyInt); (ty2, TyInt) ], TyInt)
      (* ty1、ty2がintになるという等式制約と型TyIntを返します。 *)
  | Mult ->
      ([ (ty1, TyInt); (ty2, TyInt) ], TyInt)
      (* ty1、ty2がintになるという等式制約と型TyBoolを返します。 *)
  | Lt -> ([ (ty1, TyInt); (ty2, TyInt) ], TyBool)
  | _ -> err "Not Implemented!"

(* 型環境 tyenv と式 exp を受け取って，型代入と exp の型のペアを返す *)
let rec ty_exp tyenv = function
  | Var x -> (
      try ([], Environment.lookup x tyenv)
      with Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp tyenv exp2 in
      let eqs3, ty = ty_prim op ty1 ty2 in
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 @ eqs3 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp tyenv exp2 in
      let s3, ty3 = ty_exp tyenv exp3 in
      (* s1とs2とs3を等式制約の集合に変換し、またT-Ifの条件であるty1 = TyBoolとty2 = ty3を組み合わせました。 *)
      let eqs =
        ((ty1, TyBool) :: (ty2, ty3) :: eqs_of_subst s1)
        @ eqs_of_subst s2 @ eqs_of_subst s3
      in
      (* 全体の制約を解きます。 *)
      let s4 = unify eqs in
      (s4, subst_type s4 ty2)
  | LetExp (id, exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      (* (s2, ty2)はidをty1に束縛した上で評価します。 *)
      let s2, ty2 = ty_exp (Environment.extend id ty1 tyenv) exp2 in
      (* 以下は今までのExpと同様です。 *)
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)
  | LetRecExp (f, x, exp1, exp2) ->
      (* 型付け規則のτ1をdomty1、τ2をdomty2としています。 *)
      let domty1 = TyVar (fresh_tyvar ()) in
      let domty2 = TyVar (fresh_tyvar ()) in
      (* fをτ1->τ2、xをτ1に束縛した上で(s1, ty1)を評価しています。 *)
      let s1, ty1 =
        ty_exp
          (Environment.extend x domty1
             (Environment.extend f (TyFun (domty1, domty2)) tyenv))
          exp1
      in
      (* fをτ1->τ2に束縛した上で(s2, ty2)を評価しています。 *)
      let s2, ty2 =
        ty_exp (Environment.extend f (TyFun (domty1, domty2)) tyenv) exp2
      in
      (* s1とs2を等式制約の集合に変換し、またT-LetRecの条件であるty1 = domty2を組み合わせました。 *)
      let eqs = ((ty1, domty2) :: eqs_of_subst s1) @ eqs_of_subst s2 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty = ty_exp (Environment.extend id domty tyenv) exp in
      (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp tyenv exp2 in
      (* T-Appにおけるτ2を新しくfresh_tyvarで呼んできた型変数にし、T-Appにおける制約をs1、s2の等式制約の集合と組み合わせます。 *)
      let eqs =
        ((ty1, TyFun (ty2, domty)) :: eqs_of_subst s1) @ eqs_of_subst s2
      in
      (* 全体の制約を解きます。 T-Appよりτ2を返すのでdomtyを返します。*)
      let s3 = unify eqs in
      (s3, subst_type s3 domty)
  | NilExp ->
      let domty = TyVar (fresh_tyvar ()) in
      ([], TyList domty)
  | ConsExp (exp1, exp2) ->
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, tylist2 = ty_exp tyenv exp2 in
      let eqs = ((TyList ty1, tylist2) :: eqs_of_subst s1) @ eqs_of_subst s2 in
      let s3 = unify eqs in
      (s3, subst_type s3 tylist2)
  | MatchExp (exp1, exp2, id1, id2, exp3) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s1, ty1 = ty_exp tyenv exp1 in
      let s2, ty2 = ty_exp tyenv exp2 in
      let s3, ty3 =
        ty_exp
          (Environment.extend id1 domty
             (Environment.extend id2 (TyList domty) tyenv))
          exp3
      in
      let eqs =
        ((ty1, TyList domty) :: (ty2, ty3) :: eqs_of_subst s1)
        @ eqs_of_subst s2 @ eqs_of_subst s3
      in
      let s4 = unify eqs in
      (s4, subst_type s4 ty2)
  | _ -> err "Not Implemented!"

(* ty_exp の結果から型だけを取り出す関数 *)
let ty_decl tyenv decl =
  (* 評価の返り値は型代入と型の組なので型だけを取り出します。 *)
  let ty_decl_first tyenv decl =
    match decl with Exp e -> ty_exp tyenv e | _ -> err "Not Implemented!"
  in
  let first f = match f with _, ty -> ty in
  first (ty_decl_first tyenv decl)
