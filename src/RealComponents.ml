open Base
open Expr
open Utils



let translation = [
  {
    name = "real-add";
    codomain = Type.REAL;
    domain = Type.[REAL; REAL];
    is_argument_valid = Value.(function
                        | [x ; FCall (comp, [_ ; y])]
                          when String.equal comp.name "real-sub"
                          -> x =/= y && (x =/= Const (Real 0.))
                        | [FCall (comp, [_ ; x]) ; y]
                          when String.equal comp.name "real-sub"
                          -> x =/= y && (y =/= Const (Real 0.))
                        | [x ; y] -> (x =/= Const (Real 0.)) && (y =/= Const (Real 0.))
                        | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Real (v1 +. v2));
    to_string = (fun [@warning "-8"] [a ; b] -> "(+ " ^ a ^ " " ^ b ^ ")");
    global_constraints = (fun _ -> [])
  } ;
  {
    name = "real-sub";
    codomain = Type.REAL;
    domain = Type.[REAL; REAL];
    is_argument_valid = Value.(function
                        | [(FCall (comp, [x ; y])) ; z]
                          when String.equal comp.name "real-add"
                          -> x =/= z && y =/= z && (z =/= Const (Real 0.))
                        | [(FCall (comp, [x ; _])) ; y]
                          when String.equal comp.name "real-sub"
                          -> x =/= y && (y =/= Const (Real 0.))
                        | [x ; (FCall (comp, [y ; _]))]
                          when (String.equal comp.name "real-sub" || String.equal comp.name "real-add")
                          -> x =/= y
                        | [x ; y] -> (x =/= y)
                          && (x =/= Const (Real 0.)) && (y =/= Const (Real 0.))
                        | _ -> false);                
    evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Real (v1 -. v2));
    to_string = (fun [@warning "-8"] [a ; b] -> "(- " ^ a ^ " " ^ b ^ ")");
    global_constraints = (fun _ -> [])
  }
]

let scaling = [
  {
    name = "real-mult";
    codomain = Type.REAL;
    domain = Type.[REAL; REAL];
    is_argument_valid = Value.(function
                       | [x ; y]
                         -> (x =/= Const (Real 0.)) && (x =/= Const (Real 1.)) && (x =/= Const (Real (-1.)))
                         && (y =/= Const (Real 0.)) && (y =/= Const (Real 1.)) && (x =/= Const (Real (-1.)))
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Real (v1 *. v2));
    to_string = (fun [@warning "-8"] [a ; b] -> "(* " ^ a ^ " " ^ b ^ ")");
    global_constraints = (fun _ -> [])
  } ;
  {
    name = "real-div";
    codomain = Type.REAL;
    domain = Type.[REAL; REAL];
    is_argument_valid = Value.(function
                       | [x ; y] -> x =/= y
                                 && (x =/= Const (Real 0.)) && (x =/= Const (Real 1.)) && (x =/= Const (Real (-1.)))
                                 && (y =/= Const (Real 0.)) && (y =/= Const (Real 1.)) && (y =/= Const (Real (-1.)))
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Real (v1 /. v2));
    to_string = (fun [@warning "-8"] [a ; b] -> "(/ " ^ a ^ " " ^ b ^ ")");
    global_constraints = (fun [@warning "-8"] [_ ; b] -> ["(not (= 0 " ^ b ^ "))"]);
  }
]

let linear = [
    {
      name = "real-lin-div";
      codomain = Type.REAL;
      domain = Type.[REAL; REAL];
      is_argument_valid = Value.(function
                         | [x ; y] -> x =/= y
                                   && (x =/= Const (Real 0.)) && (x =/= Const (Real 1.)) && (x =/= Const (Real (-1.)))
                                   && (y =/= Const (Real 0.)) && (y =/= Const (Real 1.)) && (y =/= Const (Real (-1.)))
                                   && ((is_constant y))
                         | _ -> false);
      evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Real (v1 /. v2));
      to_string = (fun [@warning "-8"] [a ; b] -> "(/ " ^ a ^ " " ^ b ^ ")");
      global_constraints = (fun [@warning "-8"] [_ ; b] -> ["(not (= 0 " ^ b ^ "))"]);
    } ;
    {
    name = "real-lin-mult";
    codomain = Type.REAL;
    domain = Type.[REAL; REAL];
    is_argument_valid = Value.(function
                               | [x ; y]
                                 -> (x =/= Const (Real 0.)) && (x =/= Const (Real 1.))
                                 && (y =/= Const (Real 0.)) && (y =/= Const (Real 1.))
                                 && (is_constant x || is_constant y)
                               | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Real (v1 *. v2));
    to_string = (fun [@warning "-8"] [a ; b] -> "(* " ^ a ^ " " ^ b ^ ")");
    global_constraints = (fun _ -> [])
  }
  
]

let conditionals = [
  {
    name = "real-eq";
    codomain = Type.BOOL;
    domain = Type.[REAL; REAL];
    is_argument_valid = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Bool Float.(equal v1 v2));
    to_string = (fun [@warning "-8"] [a ; b] -> "(= " ^ a ^ " " ^ b ^ ")");
    global_constraints = (fun _ -> [])
  } ;
  {
    name = "real-geq";
    codomain = Type.BOOL;
    domain = Type.[REAL; REAL];
    is_argument_valid = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Bool Float.(v1 >= v2));
    to_string = (fun [@warning "-8"] [a ; b] -> "(>= " ^ a ^ " " ^ b ^ ")");
    global_constraints = (fun _ -> [])
  } ;
  {
    name = "real-leq";
    codomain = Type.BOOL;
    domain = Type.[REAL; REAL];
    is_argument_valid = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Real v1 ; Real v2] -> Bool Float.(v1 <= v2));
    to_string = (fun [@warning "-8"] [a ; b] -> "(<= " ^ a ^ " " ^ b ^ ")");
    global_constraints = (fun _ -> [])
  } 

]


let levels = [| translation ; conditionals ; linear |]

let no_bool_levels = [| translation ; scaling |]
