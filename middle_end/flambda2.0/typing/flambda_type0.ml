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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (Term_language_function_declaration : Expr_std.S) = struct
  type t = unit
  type flambda_type = t

  module Parameters = struct
    type t = unit

    let print _ppf _t = ()

    let print_with_cache ~cache:_ ppf t = print ppf t

    let invariant _env _t = ()

    let free_names _t = Name_occurrences.empty

    let apply_name_permutation t _perm = t

    let empty = ()

    let continuation_counts _t = Continuation_counts.empty
  end

  module Typing_env_extension = struct
    type t = unit

    let print _ppf _t = ()

    let print_with_cache ~cache:_ ppf t = print ppf t

    let invariant _env _t = ()

    let free_names _t = Name_occurrences.empty

    let apply_name_permutation t _perm = t

    let continuation_counts _t = Continuation_counts.empty

    let empty = ()

    let is_empty t =
      t == empty
  end

  let print _ppf _t = ()

  let print_with_cache ~cache:_ ppf t = print ppf t

  let invariant _env _t = ()

  let free_names _t = Name_occurrences.empty

  let apply_name_permutation t _perm = t

  let continuation_counts _t = Continuation_counts.empty

  let any_value () = ()

  let any_fabricated () = ()

  let any_tagged_immediate () = ()
  let any_tagged_bool () = ()

  let any_boxed_float () = ()
  let any_boxed_int32 () = ()
  let any_boxed_int64 () = ()
  let any_boxed_nativeint () = ()

  let any_naked_immediate () = ()
  let any_naked_float () = ()
end
