open Syntax

(* 式を評価した値を表す型 *)
type exval =
  | IntV of int
  | BoolV of bool
  (* 関数閉包を表す型 *)
  | ProcV of id * exp * dnval Environment.t ref
  | DProcV of id * exp
  (* エラーが起きた際もインタプリタへの入力を受け付けるために評価時の型にエラー用の値ExceptVも含めています。 ExceptVの引数にはエラー文をとります*)
  | ExceptV of string
  | NilV
  | ConsV of exval * exval

and dnval = exval

(* エラー文からなるエラーを宣言 *)
exception Error of string

(* エラーを発生させる関数 *)
let err s = raise (Error s)

(* pretty printing *)
(* 式を評価した値を string に変換する関数 *)
let rec string_of_exval = function
  | IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (_, _, _) -> "<fun>"
  | DProcV (_, _) -> "<dfun>"
  (* エラー時の値ExceptVを出力する際は元のエラー文に"ERROR: "を付けます。 *)
  | ExceptV s -> "ERROR: " ^ s
  | NilV -> "[]"
  | ConsV (i, e) -> string_of_exval i ^ " :: " ^ string_of_exval e

(* 式を評価した値を出力する関数 *)
let pp_val v = print_string (string_of_exval v)

(* 二項演算を評価する関数 *)
let rec apply_prim op arg1 arg2 =
  match (op, arg1, arg2) with
  | Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err "Both arguments must be integer: +"
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err "Both arguments must be integer: *"
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err "Both arguments must be integer: <"
  (* &&と||はそのままOCamlの&&、||に対応させています。ただし、引数にbool以外の型をとる場合にはエラーを出力します。
     また、短絡評価によって第一引数のみで返り値が決定する場合についてはeval_expの時点で決定されるようになっています。 *)
  | Andand, BoolV i1, BoolV i2 -> BoolV (i1 && i2)
  | Andand, _, _ -> err "Both arguments must be boolean: &&"
  | Barbar, BoolV i1, BoolV i2 -> BoolV (i1 || i2)
  | Barbar, _, _ -> err "Both arguments must be boolean: ||"

(* 式を評価する関数、現在の環境と式を引数にとる *)
let rec eval_exp env = function
  | Var x -> (
      (* 式を評価した結果が変数の場合、環境からその変数の値を取ってくる *)
      try Environment.lookup x env
      with Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> (
      (* &&の第一引数にfalse、||の第一引数にtrueが来た場合は第二引数を評価する前にそれぞれfalse、trueを返す短絡評価を実装しています。
         その他の場合はapply_primで評価するようにしています。 *)
      match op with
      | Andand ->
          let check = eval_exp env exp1 in
          if check = BoolV false then BoolV false
          else
            let arg1 = eval_exp env exp1 in
            let arg2 = eval_exp env exp2 in
            apply_prim op arg1 arg2
      | Barbar ->
          let check = eval_exp env exp1 in
          if check = BoolV true then BoolV true
          else
            let arg1 = eval_exp env exp1 in
            let arg2 = eval_exp env exp2 in
            apply_prim op arg1 arg2
      | _ ->
          let arg1 = eval_exp env exp1 in
          let arg2 = eval_exp env exp2 in
          apply_prim op arg1 arg2)
  | IfExp (exp1, exp2, exp3) -> (
      let test = eval_exp env exp1 in
      match test with
      | BoolV true -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> err "Test expression must be boolean: if")
  | LetExp (id, exp1, exp2) ->
      (* let 式の場合、id を e1 に束縛してから e2　を評価する *)
      let value = eval_exp env exp1 in
      eval_exp (Environment.extend id value env) exp2
  (* let recを実装するにあたって環境が参照となったのでenvにrefを付けています。 *)
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  (* dfun用のクロージャ的なものです。宣言時での環境は必要ないので含めていません。 *)
  | DFunExp (id, exp) -> DProcV (id, exp)
  | AppExp (exp1, exp2) -> (
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      match funval with
      | ProcV (id, body, env') ->
          (* let recを実装するにあたって環境が参照となったのでenv'に!を付けています。 *)
          (* 静的束縛の場合、fun 式の宣言時点での環境を使って評価を行う *)
          let newenv = Environment.extend id arg !env' in
          eval_exp newenv body
          (* 環境は関数呼び出し時点での環境にidをargに束縛した環境を加えています。 *)
      | DProcV (id, body) ->
          let newenv = Environment.extend id arg env in
          eval_exp newenv body
      | _ -> err "Non-function value is applied")
  | LetRecExp (id, para, exp1, exp2) ->
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
      dummyenv := newenv;
      eval_exp newenv exp2
  | NilExp -> NilV
  | ConsExp (exp1, exp2) ->
      let head = eval_exp env exp1 in
      let rest = eval_exp env exp2 in
      ConsV (head, rest)
  | MatchExp (exp1, exp2, id1, id2, exp3) -> (
      if id1 = id2 then err "Head of list must be different from rest"
      else
        let matcharg = eval_exp env exp1 in
        let value1 = eval_exp env exp2 in
        match matcharg with
        | NilV -> value1
        | ConsV (head, rest) ->
            let newenv =
              Environment.extend id2 rest (Environment.extend id1 head env)
            in
            eval_exp newenv exp3
        | _ -> err "Value after match must be Nil or Cons")

(* 式・宣言を評価した際の、束縛された変数・新しい環境・束縛された変数の値を求める関数 *)
let eval_decl env = function
  | Exp e ->
      let v = eval_exp env e in
      ("-", env, v)
  | Decl (id, e) ->
      let v = eval_exp env e in
      (id, Environment.extend id v env, v)
  | RecDecl (id, para, e) ->
      (* eval_expのLetRecExpと同様にダミーの環境を与えてそこに再帰の部分を束縛した環境を破壊的に代入することで環境の循環構造を構築しています。
         また、変数idは先に再帰部分を束縛してから、値に束縛しています *)
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (para, e, dummyenv)) env in
      dummyenv := newenv;
      (id, newenv, ProcV (para, e, ref newenv))
  (* 構文解析の段階でエラーが出た場合にエラーを受け取ってそれに対応したExceptを返します。 *)
  | Exception id -> ("", env, ExceptV id)