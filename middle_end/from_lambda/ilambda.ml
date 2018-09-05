(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2017 OCamlPro SAS                                    *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42-49"]

module L = Lambda

type trap_action =
  | Push of { exn_handler : Continuation.t; }
  | Pop of { exn_handler : Continuation.t; }

type t =
  | Let of Ident.t * named * t
  | Let_mutable of let_mutable
  | Let_rec of (Ident.t * function_declaration) list * t
  | Let_cont of let_cont
  | Apply of apply
  | Apply_cont of Continuation.t * trap_action option * Ident.t list
  | Switch of Ident.t * switch
  | Event of t * L.lambda_event

and named =
  | Var of Ident.t
  | Const of L.structured_constant
  | Prim of { prim : L.primitive; args : Ident.t list; loc : Location.t;
              exception_continuation : Continuation.t; }
  | Assign of { being_assigned : Ident.t; new_value : Ident.t; }

and let_mutable = {
  id : Ident.t;
  initial_value : Ident.t;
  contents_kind : L.value_kind;
  body : t;
}

and function_declaration =
  { kind : L.function_kind;
    continuation_param : Continuation.t;
    exn_continuation_param : Continuation.t;
    params : (Ident.t * Lambda.value_kind) list;
    return : Lambda.value_kind;
    body : t;
    attr : L.function_attribute;
    loc : Location.t;
    free_idents_of_body : Ident.Set.t;
    stub : bool;
  }

and let_cont = {
  name : Continuation.t;
  administrative : bool;
  is_exn_handler : bool;
  params : Ident.t list;
  recursive : Asttypes.rec_flag;
  body : t;
  handler : t;
}

and apply =
  { kind : apply_kind;
    func : Ident.t;
    args : Ident.t list;
    continuation : Continuation.t;
    exn_continuation : Continuation.t;
    loc : Location.t;
    should_be_tailcall : bool;
    inlined : L.inline_attribute;
    specialised : L.specialise_attribute;
  }

and apply_kind =
  | Function
  | Method of { kind : L.meth_kind; obj : Ident.t; }

and switch =
  { numconsts : int;
    consts : (int * Continuation.t) list;
    failaction : Continuation.t option;
  }

type program =
  { expr : t;
    return_continuation : Continuation.t;
    exception_continuation : Continuation.t;
  }

let rec print_function ppf
      ({ continuation_param; kind; params; body; attr; }
       : function_declaration) =
  let fprintf = Format.fprintf in
  let pr_params ppf params =
    match kind with
    | Curried ->
      List.iter (fun (param, _) -> fprintf ppf "@ %a" Ident.print param) params
    | Tupled ->
      fprintf ppf " (";
      let first = ref true in
      List.iter (fun (param, _) ->
          if !first then first := false else fprintf ppf ",@ ";
          Ident.print ppf param)
        params;
      fprintf ppf ")"
  in
  fprintf ppf "@[<2>(function<%a>%a@ %a%a)@]"
    Continuation.print continuation_param
    pr_params params
    Printlambda.function_attribute attr lam body

and print_named ppf (named : named) =
  let fprintf = Format.fprintf in
  match named with
  | Var id -> Ident.print ppf id
  | Const cst -> Printlambda.structured_constant ppf cst
  | Prim { prim; args = largs; _ } ->
    fprintf ppf "@[<2>(%a %a)@]" Printlambda.primitive prim
      Ident.print_list largs
  | Assign { being_assigned; new_value; } ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print being_assigned
      Ident.print new_value

(* CR mshinwell: Rename [lam] -> [print] *)
and lam ppf (t : t) =
  let fprintf = Format.fprintf in
  match t with
  | Apply ap ->
    let print_func_and_kind ppf func =
      match ap.kind with
      | Function -> Ident.print ppf func
      | Method { kind; obj; } ->
        Format.fprintf ppf "send%a %a#%a"
          Printlambda.meth_kind kind
          Ident.print obj
          Ident.print func
    in
    fprintf ppf "@[<2>(apply@ %a<%a> %a%a%a%a)@]"
      print_func_and_kind ap.func
      Continuation.print ap.continuation
      Ident.print_list ap.args
      Printlambda.apply_tailcall_attribute ap.should_be_tailcall
      Printlambda.apply_inlined_attribute ap.inlined
      Printlambda.apply_specialised_attribute ap.specialised
  | Let(id, arg, body) ->
    let rec letbody = function
      | Let(id, arg, body) ->
        fprintf ppf "@ @[<2>%a =@ %a@]" Ident.print id print_named arg;
        letbody body
      | expr -> expr
    in
    fprintf ppf "@[<2>(let@ @[<v 1>(@[<2>%a =@ %a@]"
      Ident.print id print_named arg;
    let expr = letbody body in
    fprintf ppf ")@]@ %a)@]" lam expr
  | Let_mutable { id; initial_value; contents_kind; body; } ->
    fprintf ppf "@[<2>(let_mutable@ @[<v 1>(@[<2>%a =%s@ %a@]"
      Ident.print id
      (Printlambda.value_kind contents_kind)
      Ident.print initial_value;
    fprintf ppf ")@]@ %a)@]" lam body
  | Let_rec(id_arg_list, body) ->
    let bindings ppf id_arg_list =
      let spc = ref false in
      List.iter (fun (id, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<2>%a@ %a@]" Ident.print id print_function l)
        id_arg_list in
    fprintf ppf
      "@[<2>(let_rec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Switch(larg, sw) ->
    let switch ppf sw =
      let spc = ref false in
      List.iter (fun (n, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>| %i -> goto %a@]"
            n Continuation.print l)
        sw.consts;
      begin match sw.failaction with
      | None  -> ()
      | Some l ->
        if !spc then fprintf ppf "@ " else spc := true;
        fprintf ppf "@[<hv 1>default:@ apply_cont %a@]" Continuation.print l
      end in
    fprintf ppf
      "@[<1>(@[<v 1>%s %a@ @[<v 0>%a@]@])@]"
      (match sw.failaction with None -> "switch*" | _ -> "switch")
      Ident.print larg
      switch sw
  | Let_cont _ ->
    let rec gather_let_conts let_conts (t : t) =
      match t with
      | Let_cont let_cont ->
        gather_let_conts (let_cont :: let_conts) let_cont.body
      | body -> List.rev let_conts, body
    in
    let let_conts, body = gather_let_conts [] t in
    let print_let_cont ppf { name; administrative; params; recursive; handler;
          body = _; is_exn_handler; } =
      fprintf ppf "@[<v 2>where %a%s%s%s%s%a%s =@ %a@]"
        Continuation.print name
        (match recursive with Nonrecursive -> "" | Recursive -> "*")
        (if administrative then "<admin>" else "")
        (if is_exn_handler then "<exn>" else "")
        (match params with [] -> "" | _ -> " (")
        Ident.print_list params
        (match params with [] -> "" | _ -> ")")
        lam handler
    in
    let pp_sep ppf () = fprintf ppf "@ " in
    fprintf ppf "@[<2>(@[<v 0>%a@;@[<v 0>%a@]@])@]"
      lam body
      (Format.pp_print_list ~pp_sep print_let_cont) let_conts
  | Apply_cont (i, trap_action, ls)  ->
    let print_trap_action ppf trap_action =
      match trap_action with
      | None -> ()
      | Some (Push { exn_handler; }) ->
        fprintf ppf "push %a then "
          Continuation.print exn_handler
      | Some (Pop { exn_handler; }) ->
        fprintf ppf "pop %a then "
          Continuation.print exn_handler
    in
    fprintf ppf "@[<2>(%aapply_cont@ %a@ %a)@]"
      print_trap_action trap_action
      Continuation.print i
      Ident.print_list ls;
  | Event(expr, ev) ->
    let kind =
      match ev.lev_kind with
      | Lev_before -> "before"
      | Lev_after _ -> "after"
      | Lev_function -> "funct-body"
      | Lev_pseudo -> "pseudo"
      | Lev_module_definition _ -> "module_definition"
    in
    fprintf ppf "@[<2>(%s %s(%i)%s:%i-%i@ %a)@]" kind
      ev.lev_loc.Location.loc_start.Lexing.pos_fname
      ev.lev_loc.Location.loc_start.Lexing.pos_lnum
      (if ev.lev_loc.Location.loc_ghost then "<ghost>" else "")
      ev.lev_loc.Location.loc_start.Lexing.pos_cnum
      ev.lev_loc.Location.loc_end.Lexing.pos_cnum
      lam expr

let print = lam
