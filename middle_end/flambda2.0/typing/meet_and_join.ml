module Make
  (E : Either_meet_or_join_intf
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension) =
struct
  let meet_or_join ?bound_name env
        (t1 : Flambda_types.t) (t2 : Flambda_types.t)
        : Flambda_types.t * Typing_env_extension.t =
    let module Meet_and_join_of_kind_value =
      Meet_and_join_value.Make (E)
    in
    let module Meet_and_join_of_kind_naked_immediate =
      Meet_and_join_naked_immediate.Make (E)
    in
    let module Meet_and_join_of_kind_naked_float =
      Meet_and_join_naked_float.Make (E)
    in
    let module Meet_and_join_of_kind_naked_int32 =
      Meet_and_join_naked_int32.Make (E)
    in
    let module Meet_and_join_of_kind_naked_int64 =
      Meet_and_join_naked_int64.Make (E)
    in
    let module Meet_and_join_of_kind_naked_nativeint =
      Meet_and_join_naked_nativeint.Make (E)
    in
    let module Meet_and_join_of_kind_fabricated =
      Meet_and_join_fabricated.Make (E)
    in
    let module Meet_and_join_value =
      Make_meet_or_join.Make (E) (Meet_and_join_of_kind_value)
    in
    let module Meet_and_join_naked_immediate =
      Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_immediate)
    in
    let module Meet_and_join_naked_float =
      Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_float)
    in
    let module Meet_and_join_naked_int32 =
      Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int32)
    in
    let module Meet_and_join_naked_int64 =
      Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int64)
    in
    let module Meet_and_join_naked_nativeint =
      Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_nativeint)
    in
    let module Meet_and_join_fabricated =
      Make_meet_or_join.Make (E) (Meet_and_join_of_kind_fabricated)
    in
    if Join_env.shortcut_precondition env
      && Type_equality.fast_equal (Join_env.central_typing_environment env)
           (Join_env.central_typing_environment env) t1 t2
    then t1, Typing_env_extension.empty ()
    else begin
      Join_env.invariant env;
      let t, env_extension =
        match t1, t2 with
        | Value ty_value1, Value ty_value2 ->
          let ty_value, env_extension =
            Meet_and_join_value.meet_or_join_ty ?bound_name env
              ty_value1 ty_value2
          in
          if ty_value == ty_value1 then t1, env_extension
          else if ty_value == ty_value2 then t2, env_extension
          else Flambda_types.Value ty_value, env_extension
        | Naked_number (ty_naked_number1, kind1),
            Naked_number (ty_naked_number2, kind2) ->
          let module N = K.Naked_number in
          begin match kind1, kind2 with
          | N.Naked_immediate, N.Naked_immediate ->
            let ty_naked_number, env_extension =
              Meet_and_join_naked_immediate.meet_or_join_ty ?bound_name env
                ty_naked_number1 ty_naked_number2
            in
            if ty_naked_number == ty_naked_number1 then t1, env_extension
            else if ty_naked_number == ty_naked_number2
            then t2, env_extension
            else
              Flambda_types.Naked_number (ty_naked_number,
                  N.Naked_immediate),
                env_extension
          | N.Naked_float, N.Naked_float ->
            let ty_naked_number, env_extension =
              Meet_and_join_naked_float.meet_or_join_ty ?bound_name env
                ty_naked_number1 ty_naked_number2
            in
            if ty_naked_number == ty_naked_number1 then t1, env_extension
            else if ty_naked_number == ty_naked_number2
            then t2, env_extension
            else
              Flambda_types.Naked_number (ty_naked_number, N.Naked_float),
                env_extension
          | N.Naked_int32, N.Naked_int32 ->
            let ty_naked_number, env_extension =
              Meet_and_join_naked_int32.meet_or_join_ty ?bound_name env
                ty_naked_number1 ty_naked_number2
            in
            if ty_naked_number == ty_naked_number1 then t1, env_extension
            else if ty_naked_number == ty_naked_number2
            then t2, env_extension
            else
              Flambda_types.Naked_number (ty_naked_number, N.Naked_int32),
                env_extension
          | N.Naked_int64, N.Naked_int64 ->
            let ty_naked_number, env_extension =
              Meet_and_join_naked_int64.meet_or_join_ty ?bound_name env
                ty_naked_number1 ty_naked_number2
            in
            if ty_naked_number == ty_naked_number1 then t1, env_extension
            else if ty_naked_number == ty_naked_number2
            then t2, env_extension
            else
              Flambda_types.Naked_number (ty_naked_number, N.Naked_int64),
                env_extension
          | N.Naked_nativeint, N.Naked_nativeint ->
            let ty_naked_number, env_extension =
              Meet_and_join_naked_nativeint.meet_or_join_ty ?bound_name env
                ty_naked_number1 ty_naked_number2
            in
            if ty_naked_number == ty_naked_number1 then t1, env_extension
            else if ty_naked_number == ty_naked_number2
            then t2, env_extension
            else
              Flambda_types.Naked_number (ty_naked_number,
                  N.Naked_nativeint),
                env_extension
          | _, _ ->
            Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
              (E.name ())
              Type_printers.print t1
              Type_printers.print t2
          end
        | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
          let ty_fabricated, env_extension =
            Meet_and_join_fabricated.meet_or_join_ty ?bound_name env
              ty_fabricated1 ty_fabricated2
          in
          if ty_fabricated == ty_fabricated1 then
            t1, env_extension
          else if ty_fabricated == ty_fabricated2 then
            t2, env_extension
          else
            Flambda_types.Fabricated ty_fabricated, env_extension
        | (Value _ | Naked_number _ | Fabricated _), _ ->
          Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
            (E.name ())
            Type_printers.print t1
            Type_printers.print t2
      in
      t, env_extension
    end
end
