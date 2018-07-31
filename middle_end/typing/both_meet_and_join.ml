
  and Both_meet_and_join : Meet_and_join_intf.S_both with module T := T
    = struct
      module T = T

      let meet env t1 t2 =
        Meet.Meet_and_join.meet_or_join (Join_env.create env) t1 t2

      let join env t1 t2 =
        let join_ty, _env_extension =
          Join.Meet_and_join.meet_or_join env t1 t2
        in
        join_ty

      let as_or_more_precise env t1 ~than:t2 =
        if Type_equality.fast_equal t1 t2 then true
        else
          let meet_t, _env_extension = meet env t1 t2 in
          Type_equality.equal meet_t t1

      let strictly_more_precise env t1 ~than:t2 =
        if Type_equality.fast_equal t1 t2 then false
        else
          let meet_t, _env_extension = meet env t1 t2 in
          Type_equality.equal meet_t t1
            && not (Type_equality.equal meet_t t2)
    end
