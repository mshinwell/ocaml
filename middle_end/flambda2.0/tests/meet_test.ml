open! Flambda2

module K = Flambda_kind
module T = Flambda_type
module TE = T.Typing_env
module TEE = T.Typing_env_extension

let resolver _ = None

let test_meet_chains () =
  let comp_unit =
    Compilation_unit.create (Ident.create_persistent "Meet_test")
      (Linkage_name.create "meet_test")
  in
  Compilation_unit.set_current comp_unit;
  let env = TE.create ~resolver in
  let var1 = Variable.create "var1" in
  let var1' = Var_in_binding_pos.create var1 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var1') K.value
  in
  let env =
    TE.add_equation env (Name.var var1)
      (T.immutable_block Tag.zero ~field_kind:K.value
        ~fields:[T.any_tagged_immediate ()])
  in
  let var2 = Variable.create "var2" in
  let var2' = Var_in_binding_pos.create var2 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var2') K.value
  in
  let env =
    TE.add_equation env (Name.var var2)
      (T.alias_type_of K.value (Simple.var var1))
  in
  let symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create "my_symbol")
  in
  let env =
    TE.add_definition env (Name_in_binding_pos.symbol symbol) K.value
  in
  let env =
    TE.add_equation env (Name.symbol symbol)
      (T.alias_type_of K.value (Simple.var var2))
  in
  Format.eprintf "%a\n%!" TE.print env

let () =
  let comp_unit =
    Compilation_unit.create (Ident.create_persistent "Meet_test")
      (Linkage_name.create "meet_test")
  in
  Compilation_unit.set_current comp_unit;
  test_meet_chains ()
