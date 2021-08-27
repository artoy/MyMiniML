(* ML interpreter / type reconstruction *)
exception Error of string

let err s = raise (Error s)

type id = string

type binOp = Plus | Mult | Lt | Andand | Barbar

type exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | DFunExp of id * exp
  | AppExp of exp * exp
  (* let rec式の構文です。 *)
  | LetRecExp of id * id * exp * exp
  | NilExp
  | ConsExp of exp * exp
  | MatchExp of exp * exp * id * id * exp

type program =
  | Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp
  (* 構文解析の段階でエラーが出た場合の値です。 *)
  | Exception of id

type tyvar = int

type ty = TyInt | TyBool | TyVar of tyvar | TyFun of ty * ty | TyList of ty 

let rec freevar_ty ty =
  match ty with
  (* TyIntとTyBoolは型変数ではないので空集合を返します。 *)
  | TyInt -> MySet.empty
  | TyBool -> MySet.empty
  (* TyVarは型変数なので、そのままそれを唯一の要素とする集合を返します。 *)
  | TyVar t -> MySet.singleton t
  (* TyFunの引数中に出現する全ての型変数の集合の和集合を返します。 *)
  | TyFun (l, r) -> MySet.union (freevar_ty l) (freevar_ty r)
  | TyList t -> freevar_ty t

let rec string_of_ty ty =
  match ty with
  (* TyIntとTyBoolはそのままstring型のint,boolにします。 *)
  | TyInt -> "int"
  | TyBool -> "bool"
  (* TyVarはint型であり、0と"'a"の対応から始まり、25と"'z"まで行くと、26からは"'a1"、"'b1"...となるから、TyVarの値を26で割り、余りに対応したアルファベットと
     商の値を連結したものを返します。 *)
  | TyVar t ->
      let rec string_of_free k =
        match k with
        | 0 -> "'a"
        | 1 -> "'b"
        | 2 -> "'c"
        | 3 -> "'d"
        | 4 -> "'e"
        | 5 -> "'f"
        | 6 -> "'g"
        | 7 -> "'h"
        | 8 -> "'i"
        | 9 -> "'j"
        | 10 -> "'k"
        | 11 -> "'l"
        | 12 -> "'m"
        | 13 -> "'n"
        | 14 -> "'o"
        | 15 -> "'p"
        | 16 -> "'q"
        | 17 -> "'r"
        | 18 -> "'s"
        | 19 -> "'t"
        | 20 -> "'u"
        | 21 -> "'v"
        | 22 -> "'w"
        | 23 -> "'x"
        | 24 -> "'y"
        | 25 -> "'z"
        | n -> string_of_free (n mod 26) ^ string_of_int (n / 26)
      in
      string_of_free t
  (* 右結合で"（第一引数の型） -> （第二引数の型）"を返します。 *)
  | TyFun (l, r) -> (
      match l with
      | TyFun (_, _) -> "(" ^ string_of_ty l ^ ")" ^ " -> " ^ string_of_ty r
      | _ -> string_of_ty l ^ " -> " ^ string_of_ty r)
  | TyList t -> string_of_ty t ^ " list"

let pp_ty ty =
  (* 引数をstring_of_tyに通してstring型にしてから出力します。 *)
  print_string (string_of_ty ty)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    v
  in
  body
