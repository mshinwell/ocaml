#2 "otherlibs/dynlink/nodynlink.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*              Mark Shinwell and Leo White, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module DC = Dynlink_common

let not_available () =
  failwith "No support for native dynlink on this OS"

module Not_available = struct
  module Unit_header = struct
    type t = unit

    let defines _ = not_available ()
    let unsafe_module _ = not_available ()
  end

  type handle = unit

  let default_crcs = ref []

  let init () = not_available ()

  let is_native = false
  let adapt_filename f = f

  let iter_initial_units _ = ()

  let run _ ~unit_header:_ ~priv:_ = not_available ()
  let load ~filename:_ ~priv:_ = not_available ()
  let finish _ = not_available ()
end

include DC.Make (Not_available)

type linking_error = DC.linking_error
type error = DC.error
exception Error = DC.Error
let error_message = DC.error
