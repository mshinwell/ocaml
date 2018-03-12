(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let raise_count = ref 0

let next_raise_count () =
(*
if !raise_count = 52 then begin
Format.eprintf "Creation of continuation %d:\n%s\n%!"
  (!raise_count + 1)
  (Printexc.raw_backtrace_to_string (Printexc.get_callstack 100))
end;
*)
  incr raise_count;
  !raise_count

let reset () =
  raise_count := 0

type t = {
  id : int;
  (** [id]s are unique within any given compilation unit. *)
  compilation_unit : Compilation_unit.t
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    let c = Pervasives.compare t1.id t2.id in
    if c <> 0 then c
    else Compilation_unit.compare t1.compilation_unit t2.compilation_unit

  let equal t1 t2 =
    t1.id = t2.id
      && Compilation_unit.equal t1.compilation_unit t2.compilation_unit

  let hash t =
    Hashtbl.hash (t.id, Compilation_unit.hash t.compilation_unit)

  let print ppf t =
    if Compilation_unit.equal t.compilation_unit
        (Compilation_unit.get_current_exn ())
    then begin
      Format.fprintf ppf "k%d" t.id
    end else begin
      Format.fprintf ppf "%a.k%d"
        Compilation_unit.print t.compilation_unit
        t.id
    end
end)

let create () : t =
  { id = next_raise_count ();
    compilation_unit = Compilation_unit.get_current_exn ();
  }

let to_int t = t.id

module With_args = struct
  type nonrec t = t * Variable.t list

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      let c = compare (fst t1) (fst t2) in
      if c <> 0 then c
      else Variable.compare_lists (snd t1) (snd t2)

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t =
      Hashtbl.hash (hash (fst t),
        List.map Variable.hash (snd t))

    let print ppf (cont, vars) =
      Format.fprintf ppf "@[(%a, %a)@]"
        print cont
        Variable.print_list vars
  end)
end
