(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Construction of the interference graph.
   Annotate pseudoregs with interference lists and preference lists. *)

module IntPairSet =
  Set.Make(struct
    type t = int * int
    let compare ((a1,b1) : t) (a2,b2) =
      match compare a1 a2 with
        | 0 -> compare b1 b2
        | c -> c
  end)

open Reg
open Mach

let build_graph fundecl =

  (* The interference graph is represented in two ways:
     - by adjacency lists for each register
     - by a sparse bit matrix (a set of pairs of register stamps) *)

  let mat = ref IntPairSet.empty in

  (* Record an interference between two registers *)
  let add_interf ri rj =
    if Proc.register_class ri = Proc.register_class rj then begin
      let i = ri.shared.stamp and j = rj.shared.stamp in
      if i <> j then begin
        let p = if i < j then (i, j) else (j, i) in
        if not(IntPairSet.mem p !mat) then begin
          mat := IntPairSet.add p !mat;
          if ri.shared.loc = Unknown then begin
            ri.shared.interf <- rj.shared :: ri.shared.interf;
            if not rj.shared.spill then begin
              ri.shared.degree <- ri.shared.degree + 1
            end
          end;
          if rj.shared.loc = Unknown then begin
            rj.shared.interf <- ri.shared :: rj.shared.interf;
            if not ri.shared.spill then begin
              rj.shared.degree <- rj.shared.degree + 1
            end
          end
        end
      end
    end in

  (* Record interferences between a register array and a set of registers *)
  let add_interf_set v s =
    for i = 0 to Array.length v - 1 do
      let r1 = v.(i) in
      Reg.Set.iter (add_interf r1) s
    done in

  (* Record interferences between elements of an array *)
  let add_interf_self v =
    for i = 0 to Array.length v - 2 do
      let ri = v.(i) in
      for j = i+1 to Array.length v - 1 do
        add_interf ri v.(j)
      done
    done in

  (* Record interferences between the destination of a move and a set
     of live registers. Since the destination is equal to the source,
     do not add an interference between them if the source is still live
     afterwards. *)
  let add_interf_move src dst s =
    Reg.Set.iter (fun r ->
        if r.shared.stamp <> src.shared.stamp then add_interf dst r)
      s
  in

  (* Compute interferences *)

  let rec interf i =
    let destroyed = Proc.destroyed_at_oper i.desc in
    if Array.length destroyed > 0 then add_interf_set destroyed i.live;
    match i.desc with
      Iend -> ()
    | Ireturn -> ()
    | Iop(Imove | Ispill | Ireload) ->
        add_interf_move i.arg.(0) i.res.(0) i.live;
        interf i.next
    | Iop(Itailcall_ind) -> ()
    | Iop(Itailcall_imm _) -> ()
    | Iop _ ->
        add_interf_set i.res i.live;
        add_interf_self i.res;
        interf i.next
    | Iifthenelse(_tst, ifso, ifnot) ->
        interf ifso;
        interf ifnot;
        interf i.next
    | Iswitch(_index, cases) ->
        for i = 0 to Array.length cases - 1 do
          interf cases.(i)
        done;
        interf i.next
    | Iloop body ->
        interf body; interf i.next
    | Icatch(_, body, handler) ->
        interf body; interf handler; interf i.next
    | Iexit _ ->
        ()
    | Itrywith(body, handler) ->
        add_interf_set Proc.destroyed_at_raise handler.live;
        interf body; interf handler; interf i.next
    | Iraise _ -> () in

  (* Add a preference from one reg to another.
     Do not add anything if the two registers conflict,
     or if the source register already has a location,
     or if the two registers belong to different classes.
     (The last case can occur e.g. on Sparc when passing
      float arguments in integer registers, PR#6227.) *)

  let add_pref weight r1 r2 =
    if weight > 0 then begin
      let i = r1.shared.stamp and j = r2.shared.stamp in
      if i <> j
      && r1.shared.loc = Unknown
      && Proc.register_class r1 = Proc.register_class r2
      && (let p = if i < j then (i, j) else (j, i) in
          not (IntPairSet.mem p !mat))
      then r1.shared.prefer <- (r2.shared, weight) :: r1.shared.prefer
    end in

  (* Add a mutual preference between two regs *)
  let add_mutual_pref weight r1 r2 =
    add_pref weight r1 r2; add_pref weight r2 r1 in

  (* Update the spill cost of the registers involved in an operation *)

  let add_spill_cost cost arg =
    for i = 0 to Array.length arg - 1 do
      let r = arg.(i) in r.shared.spill_cost <- r.shared.spill_cost + cost
    done in

  (* Compute preferences and spill costs *)

  let rec prefer weight i =
    add_spill_cost weight i.arg;
    add_spill_cost weight i.res;
    match i.desc with
      Iend -> ()
    | Ireturn -> ()
    | Iop(Imove) ->
        add_mutual_pref weight i.arg.(0) i.res.(0);
        prefer weight i.next
    | Iop(Ispill) ->
        add_pref (weight / 4) i.arg.(0) i.res.(0);
        prefer weight i.next
    | Iop(Ireload) ->
        add_pref (weight / 4) i.res.(0) i.arg.(0);
        prefer weight i.next
    | Iop(Itailcall_ind) -> ()
    | Iop(Itailcall_imm _) -> ()
    | Iop _ ->
        prefer weight i.next
    | Iifthenelse(_tst, ifso, ifnot) ->
        prefer (weight / 2) ifso;
        prefer (weight / 2) ifnot;
        prefer weight i.next
    | Iswitch(_index, cases) ->
        for i = 0 to Array.length cases - 1 do
          prefer (weight / 2) cases.(i)
        done;
        prefer weight i.next
    | Iloop body ->
        (* Avoid overflow of weight and spill_cost *)
        prefer (if weight < 1000 then 8 * weight else weight) body;
        prefer weight i.next
    | Icatch(_, body, handler) ->
        prefer weight body; prefer weight handler; prefer weight i.next
    | Iexit _ ->
        ()
    | Itrywith(body, handler) ->
        prefer weight body; prefer weight handler; prefer weight i.next
    | Iraise _ -> ()
  in

  interf fundecl.fun_body; prefer 8 fundecl.fun_body
