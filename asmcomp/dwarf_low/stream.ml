(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

type t = in_channel

let open_file ~filename =
  open_in filename

let close t =
  close_in t

let read_int8 t : _ Or_error.t =
  match input_byte t with
  | exception End_of_file -> Error "End of file"
  | b -> Ok (Int8.of_int_exn b)

let read_int8_as_int t : _ Or_error.t =
  match input_byte t with
  | exception End_of_file -> Error "End of file"
  | b -> Ok b

let read_int8_as_int32 t : _ Or_error.t =
  match input_byte t with
  | exception End_of_file -> Error "End of file"
  | b -> Ok (Int32.of_int b)

let read_int8_as_int64 t : _ Or_error.t =
  match input_byte t with
  | exception End_of_file -> Error "End of file"
  | b -> Ok (Int64.of_int b)

let read_int16 t : _ Or_error.t =
  let open Or_error.Monad_infix in
  read_int8_as_int t
  >>= fun first_byte ->
  read_int8_as_int t
  >>= fun second_byte ->
  if Arch.big_endian then
    Ok ((first_byte lsl 8) lor second_byte)
  else
    Ok ((second_byte lsl 8) lor first_byte)

let read_int32 t : _ Or_error.t =
  let open Or_error.Monad_infix in
  read_int8_as_int32 t
  >>= fun first_byte ->
  read_int8_as_int32 t
  >>= fun second_byte ->
  read_int8_as_int32 t
  >>= fun third_byte ->
  read_int8_as_int32 t
  >>= fun fourth_byte ->
  if Arch.big_endian then
    Ok (Int32.logor (Int32.shift_left first_byte 24)
      (Int32.logor (Int32.shift_left second_byte 16)
        (Int32.logor (Int32.shift_left third_byte 8)
          fourth_byte)))
  else
    Ok (Int32.logor (Int32.shift_left fourth_byte 24)
      (Int32.logor (Int32.shift_left third_byte 16)
        (Int32.logor (Int32.shift_left second_byte 8)
          first_byte)))

let read_int64 t : _ Or_error.t =
  let open Or_error.Monad_infix in
  read_int8_as_int64 t
  >>= fun first_byte ->
  read_int8_as_int64 t
  >>= fun second_byte ->
  read_int8_as_int64 t
  >>= fun third_byte ->
  read_int8_as_int64 t
  >>= fun fourth_byte ->
  read_int8_as_int64 t
  >>= fun fifth_byte ->
  read_int8_as_int64 t
  >>= fun sixth_byte ->
  read_int8_as_int64 t
  >>= fun seventh_byte ->
  read_int8_as_int64 t
  >>= fun eighth_byte ->
  if Arch.big_endian then
    Ok (Int64.logor (Int64.shift_left first_byte 56)
      (Int64.logor (Int64.shift_left second_byte 48)
        (Int64.logor (Int64.shift_left third_byte 40)
          (Int64.logor (Int64.shift_left fourth_byte 32)
            (Int64.logor (Int64.shift_left fifth_byte 24)
              (Int64.logor (Int64.shift_left sixth_byte 16)
                (Int64.logor (Int64.shift_left seventh_byte 8)
                  eighth_byte)))
  else
    Ok (Int64.logor (Int64.shift_left eighth_byte 56)
      (Int64.logor (Int64.shift_left seventh_byte 48)
        (Int64.logor (Int64.shift_left sixth_byte 40)
          (Int64.logor (Int64.shift_left fifth_byte 32)
            (Int64.logor (Int64.shift_left fourth_byte 24)
              (Int64.logor (Int64.shift_left third_byte 16)
                (Int64.logor (Int64.shift_left second_byte 8)
                  first_byte)))

let read_null_terminated_string t : _ Or_error.t =
  let buf = Buffer.create 42 in
  let result = ref None in
  while !result = None do
    match read_int8_as_int t with
    | (Error _) as error -> result := Some error
    | Ok 0 -> result := Some (Buffer.contents buf)
    | Ok c -> Buffer.add_char buf c
  done;
  !result
