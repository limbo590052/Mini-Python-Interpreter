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

(* compare_value *)

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

  | Vnone, _ -> -1
  | _, Vnone -> 1
  
  | Vbool _, Vint _ | Vbool _, Vstring _ | Vbool _, Vlist _ -> -1
  | Vint _, Vbool _ | Vstring _, Vbool _ | Vlist _, Vbool _ -> 1

  | Vint _, Vstring _ | Vint _, Vlist _ -> -1
  | Vstring _, Vint _ | Vlist _, Vint _ -> 1

  | Vstring _, Vlist _ -> -1
  | Vlist _, Vstring _ -> 1
  
  | _ ->
    if v1 < v2 then -1 else 1

(* We only have global functions in Mini-Python *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* The following exception is used to interpret Python's `return` *)

exception Return of value

(* Local variables (function parameters and variables introduced by
   assignments) are stored in a hash table passed as an argument to
   the following functions under the name 'ctx' *)


type ctx = (string, value) Hashtbl.t

(* Interpreting an expression (returns a value) *)

let rec expr (ctx: ctx) = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cstring s) ->
      Vstring s
  (* arithmetic *)
  | Ecst (Cint n) ->
      assert false (* to be completed (question 1) *)
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vint n1, Vint n2 -> assert false (* to be completed (question 1) *)
        | Bsub, Vint n1, Vint n2 -> assert false (* to be completed (question 1) *)
        | Bmul, Vint n1, Vint n2 -> assert false (* to be completed (question 1) *)
        | Bdiv, Vint n1, Vint n2 -> assert false (* to be completed (question 1) *)
        | Bmod, Vint n1, Vint n2 -> assert false (* to be completed (question 1) *)
        (* to be completed (question 2) *)
        | Beq, _, _  -> Vbool (compare_value v1 v2 = 0)
        | Bneq, _, _ -> Vbool (compare_value v1 v2 <> 0)
        | Blt, _, _  -> Vbool (compare_value v1 v2 < 0)
        | Ble, _, _  -> Vbool (compare_value v1 v2 <= 0)
        | Bgt, _, _  -> Vbool (compare_value v1 v2 > 0)
        | Bge, _, _  -> Vbool (compare_value v1 v2 >= 0)
        (* to be completed (question 2) *)
        | Badd, Vstring s1, Vstring s2 ->
            assert false (* to be completed (question 3) *)
        | Badd, Vlist l1, Vlist l2 ->
            assert false (* to be completed (question 5) *)
        | _ -> error "unsupported operand types"
      end
  | Eunop (Uneg, e1) ->
      assert false (* to be completed (question 1) *)
  (* booleans *)
  | Ecst (Cbool b) ->
      assert false (* to be completed (question 2) *)
  | Ebinop (Band, e1, e2) ->
      assert false (* to be completed (question 2) *)
  | Ebinop (Bor, e1, e2) ->
      assert false (* to be completed (question 2) *)
  | Eunop (Unot, e1) ->
      assert false (* to be completed (question 2) *)
  | Eident id ->
      assert false (* to be completed (question 3) *)
  (* function call *)
  | Ecall ("len", [e1]) ->
      assert false (* to be completed (question 5) *)
  | Ecall ("list", [Ecall ("range", [e1])]) ->
      assert false (* to be completed (question 5) *)
  | Ecall (f, el) ->
      assert false (* to be completed (question 4) *)
  | Elist el ->
      assert false (* to be completed (question 5) *)
  | Eget (e1, e2) ->
      assert false (* to be completed (question 5) *)

(* Interpreting a statement (does not return anything but may raise exception `Return`) *)

and stmt (ctx: ctx) = function
  | Seval e ->
      ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e); printf "@."
  | Sblock bl ->
      block ctx bl
  | Sif (e, s1, s2) ->
      (* assert false to be completed (question 2) *)
        if is_true (expr ctx e) then
            stmt ctx s1
        else
            stmt ctx s2
  | Sassign (id, e1) ->
      assert false (* to be completed (question 3) *)
  | Sreturn e ->
      assert false (* to be completed (question 4) *)
  | Sfor (x, e, s) ->
      assert false (* to be completed (question 5) *)
  | Sset (e1, e2, e3) ->
      assert false (* to be completed (question 5) *)

(* Interpreting a block i.e. a sequence of statements *)

and block (ctx: ctx) = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* Interpreting a file
   - dl is a list of function definitions (cf Ast.def)
   - s is a statement, which represents the global statements
 *)

let file ((dl: def list), (s: stmt)) =
  (* to be completed (question 4) *)
  stmt (Hashtbl.create 16) s
