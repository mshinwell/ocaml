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

[@@@ocaml.warning "+a-30-40-41-42"]

module Make (Head : Type_head_intf.S0) = struct
  module Descr = struct
    type t =
      | No_alias of Head.t Or_unknown_or_bottom.t
      | Equals of Simple.t
      | Type of Export_id.t

    let print_with_cache ~cache ppf t =
      let colour = Flambda_colours.top_or_bottom_type () in
      match t with
      | No_alias Unknown ->
        if !Clflags.flambda2_unicode then
          Format.fprintf ppf "%s\u{22a4}%s" colour (Flambda_colours.normal ())
        else
          Format.fprintf ppf "%sT%s" colour (Flambda_colours.normal ())
      | No_alias Bottom ->
        if !Clflags.flambda2_unicode then
          Format.fprintf ppf "%s\u{22a5}%s" colour (Flambda_colours.normal ())
        else
          Format.fprintf ppf "%s_|_%s" colour (Flambda_colours.normal ())
      | No_alias (Ok head) -> Head.print_with_cache ~cache ppf head
      | Equals simple ->
        Format.fprintf ppf "@[(%s=%s %a)@]"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Simple.print simple
      | Type export_id ->
        Format.fprintf ppf "@[(%s=export_id%s %a)@]"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Export_id.print export_id

    let print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let apply_name_permutation t perm =
      if Name_permutation.is_empty perm then t
      else
        match t with
        | No_alias Bottom | No_alias Unknown -> t
        | No_alias (Ok head) ->
          let head' = Head.apply_name_permutation head perm in
          if head == head' then t
          else No_alias (Ok head')
        | Equals simple ->
          let simple' = Simple.apply_name_permutation simple perm in
          if simple == simple' then t
          else Equals simple'
        | Type _ -> t

    let free_names t =
      match t with
      | No_alias Bottom | No_alias Unknown -> Name_occurrences.empty
      | No_alias (Ok head) -> Head.free_names head
      | Equals simple -> Simple.free_names simple
      | Type _ -> Name_occurrences.empty
  end

  include With_delayed_permutation.Make (Descr)

  let print_with_cache ~cache ppf t =
    Descr.print_with_cache ~cache ppf (descr t)

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let create_no_alias head = create (No_alias head)
  let create_equals simple = create (Equals simple)
  let create_type export_id = create (Type export_id)

  let bottom () = create (No_alias Bottom)
  let unknown () = create (No_alias Unknown)

  let create head = create_no_alias (Ok head)
 
  let apply_rec_info t rec_info : _ Or_bottom.t =
    match descr t with
    | Equals simple ->
      let newer_rec_info = Some rec_info in
      begin match Simple.merge_rec_info simple ~newer_rec_info with
      | None -> Bottom
      | Some simple -> Ok (create_equals simple)
      end
    | Type _ -> Misc.fatal_error "Not yet implemented"
    | No_alias Unknown -> Ok t
    | No_alias Bottom -> Bottom
    | No_alias (Ok head) ->
      Or_bottom.map (Head.apply_rec_info head rec_info)
        ~f:(fun head -> create head)
end
