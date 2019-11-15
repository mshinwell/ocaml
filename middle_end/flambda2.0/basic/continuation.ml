(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
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

type sort =
  | Normal
  | Return
  | Define_root_symbol
  | Toplevel_return
  | Exn

type t = {
  id : int;
  (** [id]s are unique within any given compilation unit. *)
  compilation_unit : Compilation_unit.t;
  sort : sort;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    let c = Stdlib.compare t1.id t2.id in
    if c <> 0 then c
    else
      let c =
        Compilation_unit.compare t1.compilation_unit t2.compilation_unit
      in
      if c <> 0 then c
      else Stdlib.compare t1.sort t2.sort

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t =
    Hashtbl.hash (t.id, Compilation_unit.hash t.compilation_unit)

  let print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.continuation ());
    if Compilation_unit.equal t.compilation_unit
        (Compilation_unit.get_current_exn ())
    then begin
      Format.fprintf ppf "k%d" t.id
    end else begin
      Format.fprintf ppf "%a.k%d"
        Compilation_unit.print t.compilation_unit
        t.id
    end;
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let dummy =
  let compilation_unit =
    Compilation_unit.create (Ident.create_persistent "*dummy*")
      (Linkage_name.create "*dummy*")
  in
  { id = next_raise_count ();
    compilation_unit;
    sort = Normal;
  }

let create ?sort () : t =
  let sort = Option.value sort ~default:Normal in
  { id = next_raise_count ();
    compilation_unit = Compilation_unit.get_current_exn ();
    sort;
  }

let sort t = t.sort

let to_int t = t.id

let print_with_cache ~cache:_ ppf t = print ppf t

module With_args = struct
  type nonrec t = t * Variable.t list

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      let c = compare (fst t1) (fst t2) in
      if c <> 0 then c
      else Variable.compare_lists (snd t1) (snd t2)

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      Hashtbl.hash (hash (fst t),
        List.map Variable.hash (snd t))

    let print ppf (cont, vars) =
      Format.fprintf ppf "@[(%a, %a)@]"
        print cont
        Variable.print_list vars

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)
end

let is_exn t =
  match t.sort with
  | Exn -> true
  | _ -> false
