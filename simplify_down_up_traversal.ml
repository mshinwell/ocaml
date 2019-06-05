

and simplify_non_recursive_let_cont_handler env r let_cont non_rec_handler
      ~at_terminator : Expr.t * R.t =
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec_handler in
  Non_recursive_let_cont_handler.pattern_match non_rec_handler
    ~f:(fun cont ~body ->
      let body, r =
        let env = E.increment_continuation_scope_level env in
        let r = R.add_continuation r env cont in
        simplify_expr env r body ~at_terminator:(fun terminator_env r ->
          (* Lifted constants arising from simplification of the body need to
             be collected up and put in the environment before simplifying the
             handler, since the type(s) of the continuation's parameter(s) may
             involve the associated symbols. *)
          let env = E.add_lifted_constants env (R.get_lifted_constants r) in
          let cont_handler, r =
            simplify_one_continuation_handler env r cont cont_handler
          in
          let terminator_env, r = at_terminator terminator_env r in
          let terminator_env =
            if Continuation_handler.is_exn_handler cont_handler then
              E.add_continuation terminator_env cont
                (Continuation_handler.arity cont_handler)
            else
              match Continuation_handler.behaviour cont_handler with
              | Unreachable { arity; } ->
                E.add_unreachable_continuation terminator_env cont arity
              | Alias_for { arity; alias_for; } ->
                E.add_continuation_alias terminator_env cont arity ~alias_for
              | Unknown { arity; } ->
                match R.can_inline_continuation r cont with
                | None -> E.add_continuation terminator_env cont arity
                | Some non_rec_handler ->
                  E.add_continuation_to_inline terminator_env cont arity
                    (Non_recursive_let_cont_handler.handler non_rec_handler)
          in
          let r = R.add_simplified_continuation_handler r cont cont_handler in
          terminator_env, r)
      in
      let cont_handler, r = R.find_simplified_continuation_handler r cont in
      Let_cont.create_non_recursive cont cont_handler ~body, r)
