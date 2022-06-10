open Syntax
open Eval
open Typing

(* インタフェース部分を表す関数 *)
let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  (* 与えられたプログラムを抽象構文木に変換 *)
  let decl =
    try Parser.toplevel Lexer.main (Lexing.from_channel stdin) with
    (* 例外が起こった場合はException (エラー名)を返します。ただparserのエラーだけFailureで拾えなかったのでその他のエラーは全てparserによる文法エラーと
       　　見做しています。 *)
    | Failure s -> Exception s
    | _ -> Exception "syntax error"
  in
  (* 型推論を実行 *)
  let ty = ty_decl tyenv decl in
  (* 次の環境と、宣言の場合は束縛された変数とその値を求める。式の場合は次の環境と式を評価した値を求める *)
  let id, newenv, v =
    try eval_decl env decl
    with
    (* eval_decl関数は変数名、環境、束縛された値の三つ組を返すので、エラーが起こった場合には変数名を無しにして値をエラー文として出力させます。
       環境はもちろんそのままです。 *)
    | Error s ->
      ("", env, ExceptV s)
  in
  (* 宣言の場合は束縛された変数とその値、式の場合は式を評価した値を出力する *)
  Printf.printf "val %s : " id;
  pp_ty ty;
  print_string " = ";
  pp_val v;
  print_newline ();
  (* 次の入力のために環境を更新してこれらの操作を繰り返す。 *)
  read_eval_print newenv tyenv

(* 大域環境を表す let 宣言 *)
let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
       (Environment.extend "x" (IntV 10)
          (* iiを2、iiiを3、ivを4に束縛する大域環境です。 *)
          (Environment.extend "ii" (IntV 2)
             (Environment.extend "iii" (IntV 3)
                (Environment.extend "iv" (IntV 4) Environment.empty)))))

(* 大域の型環境を表す let 宣言 *)
let initial_tyenv =
  Environment.extend "i" TyInt
    (Environment.extend "v" TyInt
       (Environment.extend "x" TyInt Environment.empty))
