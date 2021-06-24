open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let threetimes = fun f -> fun x -> f (f x x) (f x x) in threetimes (+) 5;;"; expected = IntV 20};
  { input = "let threetimes = fun f -> fun x -> f (f x x) (f x x) in threetimes ( * ) 5;;"; expected = IntV 625}
];;

let () = ignore(run_test_tt_main (
    "ex3.4.2" >:::
    gen_eval_tests dataset_for_eval
  ))