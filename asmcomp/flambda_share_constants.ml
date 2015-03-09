open Abstract_identifiers

module Symbol_SCC = Sort_connected_components.Make (Symbol)

let symbol_dependency existing expr =
  let r = ref SymbolSet.empty in
  Flambdaiter.iter
    (function
      | Fsymbol (sym,_) ->
          if SymbolMap.mem sym existing
          then r := SymbolSet.add sym !r
      | _ -> ())
    expr;
  !r

let compute_sharing ~clambda_for_expr ~constants =
  let symbol_dependency_map =
    Symbol.Map.map (fun expr -> symbol_dependency P.constants expr)
      P.constants
  in
  let sorted_symbols =
    List.flatten
      (List.map (function
           | Symbol_SCC.Has_loop l -> l
           | No_loop v -> [v])
          (List.rev
             (Array.to_list
                (Symbol_SCC.connected_components_sorted_from_roots_to_leaf
                   symbol_dependency_map))))
  in
  List.fold_left (fun acc sym ->
      let lam = Symbol.Map.find sym P.constants in
      (* XXX use of [expected_symbol] *)
      let lam, ulam = fclambda_for_expr ~expected_symbol:sym lam in
      Symbol.Map.add sym (lam, structured_constant_for_symbol sym ulam) acc)
    Symbol.Map.empty sorted_symbols
