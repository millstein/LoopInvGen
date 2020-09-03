open Base

open LoopInvGen

open PIE
open Utils

let abs_job = Job.create_unlabeled
  ~f:(fun [@warning "-8"] [ Value.Int x ] -> Value.Int (if x > 0 then x else -x))
  ~args:([ "x", Type.INT ])
  ~post:(fun inp res ->
    match [@warning "-8"] inp , res with
           | [ Value.Int x ], Ok (Value.Int y) -> x = y)
  ~features:[
    ((fun [@warning "-8"] [Value.Int x] -> x > 0), "(> x 0)")
  ]
  (List.map [(-1) ; 3 ; 0 ; (-2) ; 6] ~f:(fun i -> [Value.Int i]))

let conflicts_to_string cgroup =
  let tests_to_string vs = "[" ^ (List.to_string_map ~sep:"," ~f:Value.to_string vs) ^ "]"
  in "Pos:{"
   ^ List.(to_string_map (sort cgroup.pos ~compare:Poly.compare)
                         ~sep:" ; " ~f:tests_to_string)
   ^ "} + Neg:{"
   ^ List.(to_string_map (sort cgroup.neg ~compare:Poly.compare)
                         ~sep:" ; " ~f:tests_to_string)
   ^ "}"

let abs_conflict_failure () =
  let (res, _) = learnPreCond abs_job ~config:{ PIE.Config.default with
                                                disable_synth = true }
  in Alcotest.(check string) "identical" "false" (cnf_opt_to_desc res)

let abs_conflict_group () =
  let res = List.to_string_map (conflictingTests abs_job)
                               ~sep:"\n" ~f:conflicts_to_string
  in Alcotest.(check string) "identical" "Pos:{[0]} + Neg:{[-2] ; [-1]}" res

let abs_precond_1_cnf () =
  let (res, _) = learnPreCond (Job.add_feature ~job:abs_job
                                 ((fun [@warning "-8"] [Value.Int x] -> x + x = x),
                                 "(= x (+ x x))"))
                              ~config:{ PIE.Config.default with
                                        _BFL = {
                                          BFL.Config.default with
                                          min_clauses = 1
                                        ; auto_increment_clauses = false
                                        }
                                      ; disable_synth = true
                                      }
  in Alcotest.(check string) "identical" "false" (cnf_opt_to_desc res)

let abs_precond_auto_1 () =
  let (res, _) = learnPreCond (Job.add_feature ~job:abs_job
                                 ((fun [@warning "-8"] [Value.Int x] -> x + x = x),
                                 "(= x (+ x x))"))
                               ~config:{ PIE.Config.default with
                                        _BFL = {
                                          BFL.Config.default with
                                          min_clauses = 1
                                        ; auto_increment_clauses = true
                                        }
                                      ; disable_synth = true
                                      }
  in Alcotest.(check string) "identical" "(or (= x (+ x x)) (> x 0))"
                             (cnf_opt_to_desc res)

let abs_zero_initial_features () =
  let (res, _) = learnPreCond (Job.create_unlabeled
    ~f:(fun [@warning "-8"] [ Value.Int x ] -> Value.Int (if x > 0 then x else -x))
    ~args:([ "x", Type.INT ])
    ~post:(fun inp res ->
      match [@warning "-8"] inp , res with
            | [ Value.Int x ], Ok (Value.Int y) -> x = y)
    ~features:[]
    (List.map [(-1) ; 3 ; 0 ; (-2) ; 6] ~f:(fun i -> [Value.Int i])))
  in Alcotest.(check string) "identical" "(>= x 0)" (cnf_opt_to_desc res)


 let real_abs_job = Job.create_unlabeled
  ~f:(fun [@warning "-8"] [ Value.Real x ] -> Value.Real (if Float.(x > 0.) then x else (-1. *. x)))
  ~args:([ ("x", Type.REAL) ])
  ~post:(fun inp res ->
          match [@warning "-8"] inp , res with
          | [ Value.Real x ], Ok (Value.Real y) -> Float.(equal x y))
  ~features:[
    ((fun [@warning "-8"] [Value.Real x] -> Float.(x > 0.)), "(> x 0.)")
  ]
  (List.map [(-1.4) ; 3.5 ; 0. ; (-2.6) ; 6.0] ~f:(fun i -> [Value.Real i]))

 let real_abs_conflict_failure () =
  let (res, _) = learnPreCond real_abs_job ~config:{ PIE.Config.default with
                                                disable_synth = true }
  in Alcotest.(check string) "identical" "false" (cnf_opt_to_desc res)

 let real_abs_conflict_group () =
  let res = List.to_string_map (conflictingTests real_abs_job)
                               ~sep:"\n" ~f:conflicts_to_string
  in Alcotest.(check string) "identical" "Pos:{[0.]} + Neg:{[-2.6] ; [-1.4]}" res

 let real_abs_precond_1_cnf () =
  let (res, _) = learnPreCond (Job.add_feature ~job:real_abs_job
                                 ((fun [@warning "-8"] [Value.Real x] -> Float.(equal (x +. x) x)),
                                 "(= x (+ x x))"))
                              ~config:{ PIE.Config.default with
                                        _BFL = {
                                          BFL.Config.default with
                                          min_clauses = 1
                                        ; auto_increment_clauses = false
                                        }
                                      ; disable_synth = true
                                      }
  in Alcotest.(check string) "identical" "false" (cnf_opt_to_desc res)

 let real_abs_precond_auto_1 () =
  let (res, _) = learnPreCond (Job.add_feature ~job:real_abs_job
                                 ((fun [@warning "-8"] [Value.Real x] -> Float.(equal (x +. x) x)),
                                 "(= x (+ x x))"))
                               ~config:{ PIE.Config.default with
                                        _BFL = {
                                          BFL.Config.default with
                                          min_clauses = 1
                                        ; auto_increment_clauses = true
                                        }
                                      ; disable_synth = true
                                      }
  in Alcotest.(check string) "identical" "(or (= x (+ x x)) (> x 0.))"
                             (cnf_opt_to_desc res)

let real_abs_zero_initial_features () = 
 let (res, _) = learnPreCond (Job.create_unlabeled
  ~f:(fun [@warning "-8"] [ Value.Real x ] -> Value.Real (if Float.(x > 0.) then x else (-1. *. x)))
  ~args:([ ("x", Type.REAL) ])
  ~post:(fun inp res ->
          match [@warning "-8"] inp , res with
          | [ Value.Real x ], Ok (Value.Real y) -> Float.(equal x y))
  ~features:[]
  (List.map [(-1.4) ; 3.5 ; 0. ; (-2.6) ; 6.0] ~f:(fun i -> [Value.Real i])))
    in Alcotest.(check string) "identical" "(>= x 0.)" (cnf_opt_to_desc res)



let all = [
  "Abs Conflict Failure",         `Quick, abs_conflict_failure  ;
  "Abs Conflict Group",           `Quick, abs_conflict_group    ;
  "Abs Precond 1-CNF",            `Quick, abs_precond_1_cnf ;
  "Abs Precond Auto >= 1",        `Quick, abs_precond_auto_1 ;
  "Abs No Initial Features",      `Quick, abs_zero_initial_features ;
  "Real Abs Conflict Failure",    `Quick, real_abs_conflict_failure  ;
  "Real Abs Conflict Group",      `Quick, real_abs_conflict_group    ;
  "Real Abs Precond 1-CNF",       `Quick, real_abs_precond_1_cnf ;
  "Real Abs Precond Auto >= 1",   `Quick, real_abs_precond_auto_1 ;
  "Real Abs No Initial Features", `Quick, real_abs_zero_initial_features ;
]
