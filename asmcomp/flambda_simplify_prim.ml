open Abstract_identifiers

(* Transform a [Pmakeblock] operation, that allocates and fills a new
   block, to a sequence of [let]s.  The aim is to then eliminate the
   allocation of the block, so long as it does not escape.  For example,

     Pmakeblock [expr_0; ...; expr_n]

   is transformed to:

     let x_0 = expr_0 in
     ...
     let x_n = expr_n in
     Pmakeblock [x_0; ...; x_n]

   A more general solution would be to convert completely to ANF.
*)
let lift_block_construction_to_variables t ~primitive ~args ~debuginfo
      ~current_compilation_unit =
  let block_fields, lets =
    List.fold_right (fun arg (block, lets) ->
        match arg with
        | Fvar (v, _) -> arg::block, lets
        | arg ->
          let v = Variable.create "block_field" ~current_compilation_unit in
          Fvar (v, Expr_id.create ())::block, (v, arg)::lets)
      args ([], [])
  in
  let block =
    Fprim (primitive, block_fields, debuginfo, Expr_id.create ~name:"block" ())
  in
  List.fold_left (fun body (v, expr) ->
      Flet(Not_assigned, v, expr, body, Expr_id.create ()))
    block lets

let simplify_prim exp ~current_compilation_unit =
  Flambda.iter exp ~f:(function
    | Fprim (Pidentity, [arg], _, _) -> arg
    | Fprim ((Pmakeblock _ as primitive), args, debuginfo, _) ->
      lift_block_construction_to_variables t ~env ~primitive ~args ~user_data
        ~current_compilation_unit
    | exp -> exp)
