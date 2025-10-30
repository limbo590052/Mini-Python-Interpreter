open Ast
open Format

(* Exception raised for signaling an error during interpretation *)
exception Error of string
let error s = raise (Error s)

(* Values of Mini-Python.

   Two main differences wrt Python:

   - We use here machine integers (OCaml type `int`) while Python
     integers are arbitrary-precision integers (we could use an OCaml
     library for big integers, such as zarith, but we opt for simplicity
     here).

   - What Python calls a ``list'' is a resizeable array. In Mini-Python,
     there is no way to modify the length, so a mere OCaml array can be used.
*)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Print a value on standard output *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

(* Boolean interpretation of a value

   In Python, any value can be used as a Boolean: None, the integer 0,
   the empty string, and the empty list are all considered to be
   False, and any other value to be True.
*)

(* let is_false (v: value) = assert false to be completed (question 2) *)

(* let is_true (v: value) = assert false to be completed (question 2) *)

let is_false (v: value) = 
  match v with
  | Vnone -> true
  | Vbool false -> true
  | Vint 0 -> true
  | Vstring "" -> true
  | Vlist a -> Array.length a = 0
  | _ -> false

let is_true (v: value) = not (is_false v)

(* Structural comparison (Question 7 Bonus) *)
let rec compare_value v1 v2 =
  match v1, v2 with
  | Vnone, Vnone -> 0
  | Vbool b1, Vbool b2 ->
      compare b1 b2
  | Vint n1, Vint n2 ->
      compare n1 n2
  | Vstring s1, Vstring s2 ->
      compare s1 s2
  | Vlist a1, Vlist a2 ->
      let len1 = Array.length a1 in
      let len2 = Array.length a2 in
      let rec compare_elements i =
        if i = len1 && i = len2 then 0
        else if i = len1 then -1
        else if i = len2 then 1
        else
          let c = compare_value a1.(i) a2.(i) in
          if c <> 0 then c
          else compare_elements (i + 1)
      in
      compare_elements 0

  (* Python 3-like type comparison order *)
  | Vnone, _ -> -1
  | _, Vnone -> 1
  
  | Vbool _, Vint _ | Vbool _, Vstring _ | Vbool _, Vlist _ -> -1
  | Vint _, Vbool _ | Vstring _, Vbool _ | Vlist _, Vbool _ -> 1

  | Vint _, Vstring _ | Vint _, Vlist _ -> -1
  | Vstring _, Vint _ | Vlist _, Vint _ -> 1

  | Vstring _, Vlist _ -> -1
  | Vlist _, Vstring _ -> 1
  
(* Global functions table: string -> (params as string list, body) *)
let functions = (Hashtbl.create 16 : (string, string list * stmt) Hashtbl.t)

(* The following exception is used to interpret Python's `return` *)
exception Return of value

(* Local variables (function parameters and variables introduced by
   assignments) are stored in a hash table passed as an argument to
   the following functions under the name 'ctx' *)


type ctx = (string, value) Hashtbl.t

(* Forward declarations: block / stmt / expr are mutually recursive *)
let rec block (ctx: ctx) = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

and stmt (ctx: ctx) = function
  | Seval e ->
      ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e); printf "@."
  | Sblock bl ->
      block ctx bl
  | Sif (e, s1, s2) ->
      if is_true (expr ctx e) then
        stmt ctx s1
      else
        stmt ctx s2
  
  (* assignment *)
  | Sassign (id, e1) ->
      let v = expr ctx e1 in
      let name = id.id in
      Hashtbl.replace ctx name v
  
  (* return *)
  | Sreturn e ->
      let v = expr ctx e in
      raise (Return v)
  
  (* for loop *)
  | Sfor (x, e, s) ->
      begin match expr ctx e with (* Expression e must be evaluated only once *)
      | Vlist a ->
          Array.iter (fun v ->
            Hashtbl.replace ctx x.id v;
            stmt ctx s
          ) a
      | _ -> error "for loop requires an iterable (list)"
      end
  
  (* list element assignment *)
  | Sset (e1, e2, e3) ->
      begin match expr ctx e1, expr ctx e2, expr ctx e3 with
      | Vlist a, Vint i, v3 ->
          let n = Array.length a in
          if i < 0 || i >= n then
            error "list index out of range"
          else
            a.(i) <- v3
      | Vlist _, _, _ -> error "list index must be an integer"
      | _ -> error "target of assignment must be a list"
      end

and expr (ctx: ctx) = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cstring s) ->
      Vstring s
  | Ecst (Cbool b) ->
      Vbool b
  | Ecst (Cint n) ->
      (* Convert int64 to int (assume values fit into int for this exercise) *)
      Vint (Int64.to_int n)

  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        (* Arithmetic *)
        | Badd, Vint n1, Vint n2 -> Vint (n1 + n2)
        | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2)
        | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2)
        | Bdiv, Vint n1, Vint n2 ->
            if n2 = 0 then error "division by zero" else Vint (n1 / n2)
        | Bmod, Vint n1, Vint n2 ->
            if n2 = 0 then error "division by zero" else Vint (n1 mod n2)
        
        (* Comparison *)
        | Beq, _, _  -> Vbool (compare_value v1 v2 = 0)
        | Bneq, _, _ -> Vbool (compare_value v1 v2 <> 0)
        | Blt, _, _  -> Vbool (compare_value v1 v2 < 0)
        | Ble, _, _  -> Vbool (compare_value v1 v2 <= 0)
        | Bgt, _, _  -> Vbool (compare_value v1 v2 > 0)
        | Bge, _, _  -> Vbool (compare_value v1 v2 >= 0)
        
        (* String/List Concatenation *)
        | Badd, Vstring s1, Vstring s2 ->
            Vstring (s1 ^ s2)
        | Badd, Vlist l1, Vlist l2 ->
            Vlist (Array.append l1 l2)
        | _ -> error "unsupported operand types"
      end

  | Eunop (Uneg, e1) ->
      begin match expr ctx e1 with
      | Vint n -> Vint (-n)
      | _ -> error "unsupported operand type for unary minus"
      end

  (* booleans / short-circuiting *)
  | Ebinop (Band, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_false v1 then v1 else expr ctx e2
  | Ebinop (Bor, e1, e2) ->
      let v1 = expr ctx e1 in
      if is_true v1 then v1 else expr ctx e2
  | Eunop (Unot, e1) ->
      Vbool (is_false (expr ctx e1))
  
  (* variables *)
  | Eident id ->
      let name = id.id in
      begin try
        Hashtbl.find ctx name
      with Not_found ->
        error ("variable " ^ name ^ " not found")
      end

  (* built-in functions:
     - len(x)
     - list(range(n))
  *)
  | Ecall (fname, [e1]) when fname.id = "len" ->
      begin match expr ctx e1 with
      | Vlist a -> Vint (Array.length a)
      | _ -> error "len() argument must be a list"
      end

  | Ecall (fname_list, [ Ecall (fname_range, [e1]) ]) 
      when fname_list.id = "list" && fname_range.id = "range" ->
      begin match expr ctx e1 with
      | Vint n when n >= 0 ->
          let a = Array.init n (fun i -> Vint i) in
          Vlist a
      | Vint _ -> Vlist [||]
      | _ -> error "list(range()) argument must be an integer"
      end

  (* user-defined function call *)
  | Ecall (f_ident, el) ->
      let actual_args = List.map (expr ctx) el in
      call_function f_ident.id actual_args
      
  (* list operations *)
  | Elist el ->
      let vals = List.map (expr ctx) el in
      Vlist (Array.of_list vals)
  | Eget (e1, e2) ->
      begin match expr ctx e1, expr ctx e2 with
      | Vlist a, Vint i ->
          let n = Array.length a in
          if i < 0 || i >= n then
            error "list index out of range"
          else
            a.(i)
      | _ -> error "list index must be an integer"
      end

(* Helper for function calls - defined after block/stmt/expr *)
and call_function f_name actual_args =
    try
      let (params, body) = Hashtbl.find functions f_name in
      if List.length params <> List.length actual_args then
        error "incorrect number of arguments";
      
      let new_ctx = Hashtbl.create 16 in
      List.iter2 (fun param arg_val -> 
        Hashtbl.add new_ctx param arg_val
      ) params actual_args;

      begin try
        stmt new_ctx body; (* 'block' is now visible *)
        Vnone
      with
      | Return v -> v
      end
    with Not_found ->
      error ("function " ^ f_name ^ " not defined")

(* Interpreting a file *)
let file ((dl: def list), (s: stmt)) =
  (* Fill the global function table.
     Each def is (ident, ident list, body) *)
  List.iter (fun (id, params, body) ->
    let name = id.id in
    let params_strs = List.map (fun pid -> pid.id) params in
    Hashtbl.add functions name (params_strs, body)
  ) dl;
  stmt (Hashtbl.create 16) s
