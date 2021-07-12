%{
open Fexpr

let make_loc (startpos, endpos) =
  Debuginfo.Scoped_location.of_location
    ~scopes:Debuginfo.Scoped_location.empty_scopes
    {
      Location.loc_start = startpos;
      Location.loc_end = endpos;
      Location.loc_ghost = false;
    }

let make_located txt (startpos, endpos) =
  let loc = make_loc (startpos, endpos) in
  { txt; loc }

let make_plain_int = function
  | s, None -> Int64.of_string s |> Int64.to_int
  | _, Some _ ->
    Misc.fatal_errorf "No modifier expected here"

let make_targetint = function
  | s, None -> Int64.of_string s
  | _, Some _ ->
    Misc.fatal_errorf "No modifier expected here"

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    Misc.fatal_errorf "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, None -> s
  | _, _ ->
    Misc.fatal_errorf "Must be a tagged immediate"

let make_const_int (i, m) : const =
  match m with
  | None -> Tagged_immediate i
  | Some 'i' -> Naked_immediate i
  | Some 'n' -> Naked_nativeint (Int64.of_string i)
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> Misc.fatal_errorf "Unknown int modifier: %c" c

let make_boxed_const_int (i, m) : static_data =
  match m with
  | None -> Misc.fatal_errorf "Need int modifier for static data"
  | Some 'n' -> Boxed_nativeint (Const (Int64.of_string i))
  | Some 'l' -> Boxed_int32 (Const (Int32.of_string i))
  | Some 'L' -> Boxed_int64 (Const (Int64.of_string i))
  | Some c -> Misc.fatal_errorf "Bad int modifier for static data: %c" c

%}

/* Tokens */

%token AT    [@symbol "@"]
%token BIGARROW [@symbol "===>"]
%token BLANK [@symbol "_"]
%token COLON  [@symbol ":"]
%token COMMA [@symbol ","]
%token DOT   [@symbol "."]
%token EQUAL [@symbol "="]
%token EQUALDOT [@symbol "=."]
%token <float> FLOAT
%token GREATER [@symbol ">"]
%token GREATEREQUAL [@symbol ">="]
%token GREATERDOT [@symbol ">."]
%token GREATEREQUALDOT [@symbol ">"]
%token <string> IDENT
%token <string * char option> INT
%token LBRACE [@symbol "{"]
%token LBRACKPIPE [@symbol "[|"]
%token LESS   [@symbol "<"]
%token LESSDOT [@symbol "<."]
%token LESSEQUAL [@symbol "<="]
%token LESSEQUALDOT [@symbol "<=."]
%token LESSMINUS [@symbol "<-"]
%token LPAREN [@symbol "("]
%token MINUS    [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token NOTEQUALDOT [@symbol "!=."]
%token QMARK [@symbol "?"]
%token QMARKDOT [@symbol "?."]
%token PIPE [@symbol "|"]
%token PERCENT [@symbol "%"]
%token PLUS     [@symbol "+"]
%token PLUSDOT  [@symbol "+."]
%token RBRACE [@symbol "}"]
%token RBRACKPIPE [@symbol "|]"]
%token RPAREN [@symbol ")"]
%token SEMICOLON [@symbol ";"]
%token SLASH  [@symbol "/"]
%token SLASHDOT [@symbol "/."]
%token STAR   [@symbol "*"]
%token STARDOT [@symbol "*."]
%token<string> STRING
%token<Fexpr.compilation_unit option * string> SYMBOL
%token EOF

%token KWD_ALWAYS [@symbol "always"]
%token KWD_AND   [@symbol "and"]
%token KWD_ANDWHERE [@symbol "andwhere"]
%token KWD_APPLY [@symbol "apply"]
%token KWD_ASR   [@symbol "asr"]
%token KWD_BLOCK [@symbol "Block"]
%token KWD_BOXED [@symbol "boxed"]
%token KWD_CCALL  [@symbol "ccall"]
%token KWD_CLOSURE  [@symbol "closure"]
%token KWD_CODE  [@symbol "code"]
%token KWD_CONT  [@symbol "cont"]
%token KWD_DEFAULT [@symbol "default"]
%token KWD_DEFINE_ROOT_SYMBOL [@symbol "define_root_symbol"]
%token KWD_DELETED [@symbol "deleted"]
%token KWD_DEPTH [@symbol "depth"]
%token KWD_DIRECT [@symbol "direct"]
%token KWD_DOMINATOR_SCOPED [@symbol "dominator_scoped"]
%token KWD_DONE  [@symbol "done"]
%token KWD_DYNAMIC [@symbol "dynamic"]
%token KWD_END   [@symbol "end"]
%token KWD_ERROR [@symbol "error"]
%token KWD_EXN   [@symbol "exn"]
%token KWD_FABRICATED [@symbol "fabricated"]
%token KWD_FLOAT [@symbol "float"]
%token KWD_FLOAT_ARRAY [@symbol "Float_array"]
%token KWD_FLOAT_BLOCK [@symbol "Float_block"]
%token KWD_HCF   [@symbol "halt_and_catch_fire"]
%token KWD_HINT  [@symbol "hint"]
%token KWD_IMM   [@symbol "imm" ]
%token KWD_IMMUTABLE_UNIQUE [@symbol "immutable_unique"]
%token KWD_IN    [@symbol "in"]
%token KWD_INLINE [@symbol "inline"]
%token KWD_INLINING_STATE [@symbol "inlining_state"]
%token KWD_INT32 [@symbol "int32"]
%token KWD_INT64 [@symbol "int64"]
%token KWD_LAND  [@symbol "land"]
%token KWD_LET   [@symbol "let"]
%token KWD_LOR   [@symbol "lor"]
%token KWD_LSL   [@symbol "lsl"]
%token KWD_LSR   [@symbol "lsr"]
%token KWD_LXOR  [@symbol "lxor"]
%token KWD_MUTABLE [@symbol "mutable"]
%token KWD_NATIVEINT [@symbol "nativeint"]
%token KWD_NEVER  [@symbol "never"]
%token KWD_NEWER_VERSION_OF [@symbol "newer_version_of"]
%token KWD_NOALLOC [@symbol "noalloc"]
%token KWD_NOTRACE [@symbol "notrace"]
%token KWD_POP    [@symbol "pop"]
%token KWD_PUSH   [@symbol "push"]
%token KWD_REC    [@symbol "rec"]
%token KWD_REC_INFO [@symbol "rec_info"]
%token KWD_REGULAR [@symbol "regular"]
%token KWD_RERAISE [@symbol "reraise"]
%token KWD_SET_OF_CLOSURES [@symbol "set_of_closures"]
%token KWD_SIZE   [@symbol "size"]
%token KWD_SWITCH [@symbol "switch"]
%token KWD_TAGGED [@symbol "tagged"]
%token KWD_TUPLED [@symbol "tupled"]
%token KWD_UNIT   [@symbol "unit"]
%token KWD_UNREACHABLE [@symbol "unreachable"]
%token KWD_UNROLL [@symbol "unroll"]
%token KWD_UNSIGNED [@symbol "unsigned"]
%token KWD_VAL    [@symbol "val"]
%token KWD_WHERE  [@symbol "where"]
%token KWD_WITH   [@symbol "with"]

%token PRIM_ARRAY_LENGTH [@symbol "%array_length"]
%token PRIM_ARRAY_LOAD [@symbol "%array_load"]
%token PRIM_ARRAY_SET [@symbol "%array_set"]
%token PRIM_BLOCK [@symbol "%Block"]
%token PRIM_BLOCK_LOAD [@symbol "%block_load"]
%token PRIM_BOX_FLOAT [@symbol "%Box_float"]
%token PRIM_BOX_INT32 [@symbol "%Box_int32"]
%token PRIM_BOX_INT64 [@symbol "%Box_int64"]
%token PRIM_BOX_NATIVEINT [@symbol "%Box_nativeint"]
%token PRIM_BYTES_LENGTH [@symbol "%bytes_length"]
%token PRIM_GET_TAG [@symbol "%get_tag"]
%token PRIM_INT_ARITH [@symbol "%int_arith"]
%token PRIM_INT_COMP [@symbol "%int_comp"]
%token PRIM_INT_SHIFT [@symbol "%int_shift"]
%token PRIM_IS_INT [@symbol "%is_int"]
%token PRIM_NUM_CONV [@symbol "%num_conv"]
%token PRIM_OPAQUE [@symbol "%Opaque"]
%token PRIM_PHYS_EQ [@symbol "%phys_eq"]
%token PRIM_PHYS_NE [@symbol "%phys_ne"]
%token PRIM_PROJECT_VAR [@symbol "%project_var"]
%token PRIM_SELECT_CLOSURE [@symbol "%select_closure"]
%token PRIM_STRING_LENGTH [@symbol "%string_length"]
%token PRIM_TAG_IMM [@symbol "%tag_imm"]
%token PRIM_UNBOX_FLOAT [@symbol "%unbox_float"]
%token PRIM_UNBOX_INT32 [@symbol "%unbox_int32"]
%token PRIM_UNBOX_INT64 [@symbol "%unbox_int64"]
%token PRIM_UNBOX_NATIVEINT [@symbol "%unbox_nativeint"]
%token PRIM_UNTAG_IMM [@symbol "%untag_imm"]

%start flambda_unit expect_test_spec
%type <Fexpr.array_kind> array_kind
%type <Fexpr.binary_float_arith_op> binary_float_arith_op
%type <Fexpr.binary_int_arith_op> binary_int_arith_op
%type <Fexpr.block_access_field_kind> block_access_field_kind
%type <Fexpr.const> const
%type <Fexpr.standard_int_or_float> convertible_type
%type <Fexpr.expect_test_spec> expect_test_spec
%type <Fexpr.field_of_block> field_of_block
%type <Fexpr.flambda_unit> flambda_unit
%type <Fexpr.comparison Fexpr.comparison_behaviour> float_comp
%type <Fexpr.continuation_sort option> continuation_sort
%type <float Fexpr.or_variable> float_or_variable
%type <Fexpr.infix_binop> infix_binop
%type <Fexpr.ordered_comparison Fexpr.comparison_behaviour> int_comp
%type <Fexpr.kind> kind
%type <Fexpr.kind_with_subkind> kind_with_subkind
%type <Fexpr.kind_with_subkind list> kinds_with_subkinds
%type <Fexpr.mutability> mutability
%type <Fexpr.naked_number_kind> naked_number_kind
%type <Fexpr.name> name
%type <Fexpr.named> named
%type <Fexpr.special_continuation> special_continuation
%type <Fexpr.standard_int> standard_int
%type <Fexpr.static_data> static_data
%type <Fexpr.static_data_binding> static_data_binding
%type <Fexpr.variable -> Fexpr.static_data> static_data_kind
%type <Fexpr.symbol_binding> symbol_binding
%%

(* CR-someday lmaurer: Modularize and generally clean up *)

flambda_unit:
  | body = module_
    EOF
    { body }
;

expect_test_spec:
  | before = module_; BIGARROW; after = module_; EOF
    { { before; after } }
;

(* XCR lwhite: Probably easier to just use some default names for these
   continuations

   lmaurer: Makes sense. I went with "done" and "error" for the names. *)
module_:
  | body = expr
    { { body } }
;

exn_continuation:
  | STAR cont = continuation { cont }

exn_continuation_id:
  | STAR cont = continuation_id { cont }
;

let_symbol(body):
  | KWD_LET; scoping_rule = symbol_scoping_rule;
    bindings = separated_nonempty_list(KWD_AND, symbol_binding);
    closure_elements = with_closure_elements_opt;
    KWD_IN; body = body; { { bindings; closure_elements; scoping_rule; body } }
;

symbol_scoping_rule:
  | { None }
  | KWD_DOMINATOR_SCOPED { Some Dominator }

symbol_binding:
  | s = static_data_binding { Data s }
  | code = code { Code code }
  | s = static_closure_binding { Closure s }
  | s = static_set_of_closures { Set_of_closures s }
;

code:
  | header = code_header;
    params = kinded_args;
    closure_var = variable;
    MINUSGREATER; ret_cont = continuation_id;
    exn_cont = exn_continuation_id;
    ret_arity = return_arity;
    is_tupled = boption(KWD_TUPLED);
    EQUAL; body = expr;
    { let recursive, inline, id, newer_version_of, code_size = header in
      { id; newer_version_of; param_arity = None; ret_arity; recursive; inline;
        params_and_body = Present { params; closure_var; ret_cont; exn_cont;
                                    body };
        code_size; is_tupled; } }
  | header = code_header;
    KWD_DELETED;
    COLON;
    is_tupled = boption(KWD_TUPLED);
    param_arity = kinds_with_subkinds;
    MINUSGREATER;
    ret_arity = kinds_with_subkinds;
    { let recursive, inline, id, newer_version_of, code_size = header in
      { id; newer_version_of; param_arity = Some param_arity;
        ret_arity = Some ret_arity; recursive; inline; code_size;
        params_and_body = Deleted; is_tupled; } }
;

code_header:
  | KWD_CODE;
    recursive = recursive;
    inline = option(inline);
    KWD_SIZE LPAREN; code_size = code_size; RPAREN;
    newer_version_of = option(newer_version_of);
    id = code_id;
    { recursive, inline, id, newer_version_of, code_size }
;

newer_version_of:
  | KWD_NEWER_VERSION_OF LPAREN; id = code_id; RPAREN { id };

static_closure_binding:
  | symbol = symbol; EQUAL; fun_decl = fun_decl;
    { { symbol; fun_decl } }
;

static_set_of_closures:
  | KWD_SET_OF_CLOSURES;
    bindings = separated_nonempty_list(KWD_AND, static_closure_binding);
    elements = with_closure_elements_opt;
    KWD_END
    { { bindings; elements } }

recursive:
  | { Nonrecursive }
  | KWD_REC { Recursive }
;

unop:
  | PRIM_ARRAY_LENGTH; ak = array_kind { Array_length ak }
  | PRIM_BOX_FLOAT { Box_number Naked_float }
  | PRIM_BOX_INT32 { Box_number Naked_int32 }
  | PRIM_BOX_INT64 { Box_number Naked_int64 }
  | PRIM_BOX_NATIVEINT { Box_number Naked_nativeint }
  | PRIM_BYTES_LENGTH { String_length Bytes }
  | PRIM_GET_TAG { Get_tag }
  | PRIM_IS_INT { Is_int }
  | PRIM_NUM_CONV; LPAREN;
      src = convertible_type; MINUSGREATER; dst = convertible_type;
    RPAREN
    { Num_conv { src; dst } }
  | PRIM_OPAQUE { Opaque_identity }
  | PRIM_PROJECT_VAR; project_from = closure_id; DOT; var = var_within_closure
    { Project_var { project_from; var } }
  | PRIM_SELECT_CLOSURE; LPAREN;
      move_from = closure_id; MINUSGREATER; move_to = closure_id;
    RPAREN
    { Select_closure { move_from; move_to } }
  | PRIM_STRING_LENGTH { String_length String }
  | PRIM_TAG_IMM { Box_number Untagged_immediate }
  | PRIM_UNBOX_FLOAT { Unbox_number Naked_float }
  | PRIM_UNBOX_INT32 { Unbox_number Naked_int32 }
  | PRIM_UNBOX_INT64 { Unbox_number Naked_int64 }
  | PRIM_UNBOX_NATIVEINT { Unbox_number Naked_nativeint }
  | PRIM_UNTAG_IMM { Unbox_number Untagged_immediate }

infix_binop:
  | o = binary_int_arith_op { Int_arith o }
  | c = int_comp { Int_comp c }
  | s = int_shift { Int_shift s }
  | o = binary_float_arith_op { Float_arith o }
  | c = float_comp { Float_comp c }
;

prefix_binop:
  | PRIM_BLOCK_LOAD;
    mutability = mutability;
    kind = block_access_kind;
    { Block_load (kind, mutability) }
  | PRIM_PHYS_EQ; k = kind_arg_opt { Phys_equal(k, Eq) }
  | PRIM_PHYS_NE; k = kind_arg_opt { Phys_equal(k, Neq) }

mutability:
  | KWD_MUTABLE { Mutable }
  | KWD_IMMUTABLE_UNIQUE { Immutable_unique }
  | { Immutable }

array_kind:
  | { Values }
  | KWD_IMM { Immediates }
  | KWD_FLOAT { Naked_floats }
  | KWD_DYNAMIC { Float_array_opt_dynamic }

block_access_kind:
  | field_kind = block_access_field_kind; tag = tag; size = size_opt
    { Values { field_kind; tag; size } }
  | KWD_FLOAT; size = size_opt
    { Naked_floats { size } }
;

block_access_field_kind:
  | { Any_value }
  | KWD_IMM { Immediate }

size_opt:
  | { None }
  | KWD_SIZE; LPAREN; size = targetint; RPAREN { Some size }

standard_int:
  | { Tagged_immediate }
  | KWD_IMM { Naked_immediate }
  | KWD_INT32 { Naked_int32 }
  | KWD_INT64 { Naked_int64 }
  | KWD_NATIVEINT { Naked_nativeint }

convertible_type:
  | KWD_IMM KWD_TAGGED { Tagged_immediate }
  | KWD_IMM { Naked_immediate }
  | KWD_FLOAT { Naked_float }
  | KWD_INT32 { Naked_int32 }
  | KWD_INT64 { Naked_int64 }
  | KWD_NATIVEINT { Naked_nativeint }

init_or_assign:
  | EQUAL { Initialization }
  | LESSMINUS { Assignment }

signed_or_unsigned:
  | { Signed }
  | KWD_UNSIGNED { Unsigned }

binary_int_arith_op:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }
  | PERCENT { Mod }
  | KWD_LAND { And }
  | KWD_LOR { Or }
  | KWD_LXOR { Xor }

binary_float_arith_op:
  | PLUSDOT { Add }
  | MINUSDOT { Sub }
  | STARDOT { Mul }
  | SLASHDOT { Div }

int_comp:
  | LESS { Yielding_bool Lt }
  | GREATER { Yielding_bool Gt }
  | LESSEQUAL { Yielding_bool Le }
  | GREATEREQUAL { Yielding_bool Ge }
  | QMARK { Yielding_int_like_compare_functions }

float_comp:
  | EQUALDOT { Yielding_bool Eq }
  | NOTEQUALDOT { Yielding_bool Neq }
  | LESSDOT { Yielding_bool Lt }
  | GREATERDOT { Yielding_bool Gt }
  | LESSEQUALDOT { Yielding_bool Le }
  | GREATEREQUALDOT { Yielding_bool Ge }
  | QMARKDOT { Yielding_int_like_compare_functions }
;

int_shift:
  | KWD_LSL { Lsl }
  | KWD_LSR { Lsr }
  | KWD_ASR { Asr }
;

binop_app:
  | op = prefix_binop; LPAREN; arg1 = simple; COMMA; arg2 = simple; RPAREN
    { Binary (op, arg1, arg2) }
  | arg1 = simple; op = infix_binop; arg2 = simple
    { Binary (Infix op, arg1, arg2) }
  | PRIM_ARRAY_LOAD; ak = array_kind; mut = mutability;
    arg1 = simple; DOT; LPAREN; arg2 = simple; RPAREN
    { Binary (Array_load (ak, mut), arg1, arg2) }
  | PRIM_INT_ARITH; i = standard_int;
    arg1 = simple; c = binary_int_arith_op; arg2 = simple
    { Binary (Int_arith (i, c), arg1, arg2) }
  | PRIM_INT_COMP;
    i = standard_int; s = signed_or_unsigned;
    arg1 = simple; c = int_comp; arg2 = simple
    { Binary (Int_comp (i, s, c), arg1, arg2) }
  | PRIM_INT_SHIFT;
    i = standard_int; arg1 = simple; s = int_shift; arg2 = simple
    { Binary (Int_shift (i, s), arg1, arg2) }
;

ternop_app:
  | PRIM_ARRAY_SET; ak = array_kind;
    arr = simple; DOT LPAREN; ix = simple; RPAREN; ia = init_or_assign;
    v = simple
    { Ternary (Array_set (ak, ia), arr, ix, v) }
;

block:
  | PRIM_BLOCK; m = mutability; t = tag; LPAREN;
    elts = separated_list(COMMA, simple);
    RPAREN
    { Variadic (Make_block (t, m), elts) }
;

named:
  | s = simple { Simple s }
  | u = unop a = simple { Prim (Unary (u, a)) }
  | b = binop_app { Prim b }
  | t = ternop_app { Prim t }
  | b = block { Prim b }
  | c = fun_decl { Closure c }
;

switch_case:
  | i = tag; MINUSGREATER; ac = apply_cont_expr { i,ac }
;

switch:
  | option(PIPE); cs = separated_list(PIPE, switch_case) { cs }
;
naked_number_kind:
  | KWD_IMM { Naked_immediate }
  | KWD_FLOAT { Naked_float }
  | KWD_INT32 { Naked_int32 }
  | KWD_INT64 { Naked_int64 }
  | KWD_NATIVEINT { Naked_nativeint }
;
kind:
  | KWD_VAL { Value }
  | nnk = naked_number_kind { Naked_number nnk }
  | KWD_FABRICATED { Fabricated }
  | KWD_REC_INFO { Rec_info }
;
kind_with_subkind:
  | KWD_VAL { Any_value }
  | nnk = naked_number_kind { Naked_number nnk }
  | KWD_FLOAT KWD_BOXED { Boxed_float }
  | KWD_INT32 KWD_BOXED { Boxed_int32 }
  | KWD_INT64 KWD_BOXED { Boxed_int64 }
  | KWD_NATIVEINT KWD_BOXED { Boxed_nativeint }
  | KWD_IMM KWD_TAGGED { Tagged_immediate }
;
kinds_with_subkinds :
  | KWD_UNIT { [] }
  | ks = separated_nonempty_list(STAR, kind_with_subkind) { ks }
;
return_arity:
  | { None }
  | COLON k = kinds_with_subkinds { Some k }
;
kind_arg_opt:
  | { None }
  | LBRACE; k = kind; RBRACE { Some k }
;

/* expr is staged so that let and where play nicely together. In particular, in
   let ... in ... where, we want the where to be on the inside so that the
   continuation can refer to the let-bound variables (and the defining
   expressions can't refer to continuations anyway); and in where ... where, we
   want both wheres to be at the same level (as it's easier to use parens to
   force them to nest than it is force them to un-nest). The straightforward way
   to achieve this is to have an expression be a let expression or an inner
   expression, and to have where be an inner expression. Then we say that the
   body of a continuation can have let but can't have where (without
   parentheses, that is). Unfortunately, it's hard to say "a let whose body is
   not a where" in a grammar, but we can get close by parameterizing let_expr by
   what nonterminal its body should be. */

expr:
  | l = let_expr(expr) { l }
  | i = inner_expr { i }
;

let_expr(body):
  | KWD_LET l = let_(body) { Let l }
  | ls = let_symbol(body) { Let_symbol ls }
;

inner_expr:
  | w = where_expr { w }
  | a = atomic_expr { a }
;

where_expr:
  | body = inner_expr; KWD_WHERE; recursive = recursive;
    bindings = separated_list(KWD_ANDWHERE, continuation_binding)
    { Let_cont { recursive; body; bindings } }
;

continuation_body:
  | l = let_expr(continuation_body) { l }
  | a = atomic_expr { a }
;

atomic_expr:
  | KWD_HCF { Invalid Halt_and_catch_fire }
  | KWD_UNREACHABLE { Invalid Treat_as_unreachable }
  | KWD_CONT; ac = apply_cont_expr { Apply_cont ac }
  | KWD_SWITCH; scrutinee = simple; cases = switch { Switch {scrutinee; cases} }
  | KWD_APPLY e = apply_expr { Apply e }
  | LPAREN; e = expr; RPAREN { e }
;

let_(body):
  | bindings = separated_nonempty_list(KWD_AND, let_binding);
(*  CR lwhite: I think this closure elements stuff is a bit of a hangover from
    when closures definitions contained the code as well. I imagine the closures
    used to look like:

    let f a b c =
      ...
    and g x y z =
      ...
    with { i = j; ... } in
    ...

    but now they should probably just look something like:

      let (f', g') = closure({f, g}, {i = j; ...}) in
      ...

    lmaurer: Let_symbol_expr.t still allows code and closure definitions to be
    mutually recursive, though, so we need some syntax that bundles them
    together. Also, several closures can share the same closure elements.
 *)
    closure_elements = with_closure_elements_opt;
    KWD_IN body = body;
    { ({ bindings; closure_elements; body } : let_) }
;

let_binding:
  | var = variable EQUAL defining_expr = named
    { { var; defining_expr } }
;

with_closure_elements_opt:
  | { None }
  | KWD_WITH LBRACE;
      elements = separated_list(SEMICOLON, closure_element);
    RBRACE;
    { Some elements }
;

closure_element:
  | var = var_within_closure; EQUAL; value = simple; { { var; value; } }
;

fun_decl:
  | KWD_CLOSURE; code_id = code_id;
    closure_id = closure_id_opt;
    { { code_id; closure_id; } }
;

apply_expr:
  | call_kind = call_kind;
    inline = option(inline);
    inlining_state = option(inlining_state);
    func = func_name_with_optional_arities
    args = simple_args MINUSGREATER
    r = result_continuation e = exn_continuation
     { let (func, arities) = func in {
       func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind;
          inline;
          inlining_state;
          arities;
     } }
;

call_kind:
  | { Function Indirect }
  | KWD_DIRECT; LPAREN; code_id = code_id; closure_id = closure_id_opt; RPAREN
    { Function (Direct { code_id; closure_id }) }
  | KWD_CCALL; noalloc = boption(KWD_NOALLOC)
    { C_call { alloc = not noalloc } }
;

inline:
  | KWD_INLINE LPAREN KWD_ALWAYS RPAREN { Always_inline }
  | KWD_INLINE LPAREN KWD_HINT RPAREN { Hint_inline }
  | KWD_INLINE LPAREN KWD_NEVER RPAREN { Never_inline }
  | KWD_UNROLL LPAREN; i = plain_int; RPAREN { Unroll i }
  | KWD_INLINE LPAREN KWD_DEFAULT RPAREN { Default_inline }

inlining_state:
  | KWD_INLINING_STATE LPAREN; depth = inlining_state_depth; RPAREN
    {
      (* CR poechsel: Parse the inlining arguments *)
      { depth }
    }

inlining_state_depth:
  | KWD_DEPTH LPAREN; i = plain_int; RPAREN { i }

result_continuation:
  | c = continuation { Return c }
  | KWD_NEVER { Never_returns }
;

apply_cont_expr:
  | cont = continuation; trap_action = option(trap_action); args = simple_args
    { { cont; args; trap_action } }
;

trap_action:
  | KWD_PUSH; LPAREN; exn_handler = continuation; RPAREN { Push { exn_handler } }
  | KWD_POP; LPAREN;
      raise_kind = option(raise_kind); exn_handler = continuation;
    RPAREN
    { Pop { exn_handler; raise_kind } }
;

raise_kind:
  | KWD_REGULAR { Regular }
  | KWD_RERAISE { Reraise }
  | KWD_NOTRACE { No_trace }

continuation_sort:
  | { None }
  | KWD_EXN { Some Exn }
  | KWD_DEFINE_ROOT_SYMBOL { Some Define_root_symbol }
;

continuation_binding:
  | name = continuation_id; sort = continuation_sort;
    params = kinded_args; EQUAL; handler = continuation_body
    { { name; params; handler; sort } }
;

kinded_args:
  | { [] }
  | LPAREN; vs = separated_nonempty_list(COMMA, kinded_variable); RPAREN { vs }
;

static_data_binding:
  | s = symbol EQUAL sp = static_data
    { { symbol = s; defining_expr = sp } }
;

static_data:
  | KWD_BLOCK; m = mutability; tag = tag; LPAREN;
    elements = separated_list(COMMA, field_of_block); RPAREN
    { (Block { tag; mutability = m; elements } : static_data) }
  | f = FLOAT { Boxed_float (Const f) }
  | i = INT { make_boxed_const_int i }
  | v = variable; COLON; k = static_data_kind { k v }
  | KWD_FLOAT_BLOCK; LPAREN;
    fs = separated_list(COMMA, float_or_variable);
    RPAREN
    { Immutable_float_block fs }
  | KWD_FLOAT_ARRAY; LBRACKPIPE;
    fs = separated_list(SEMICOLON, float_or_variable);
    RBRACKPIPE
    { Immutable_float_array fs }
  | KWD_MUTABLE; s = STRING { Mutable_string { initial_value = s } }
  | s = STRING { Immutable_string s }
;

static_data_kind:
  | KWD_FLOAT KWD_BOXED { fun v -> Boxed_float (Var v) }
  | KWD_INT32 KWD_BOXED { fun v -> Boxed_int32 (Var v) }
  | KWD_INT64 KWD_BOXED { fun v -> Boxed_int64 (Var v) }
  | KWD_NATIVEINT KWD_BOXED { fun v -> Boxed_nativeint (Var v) }

float_or_variable:
  | f = FLOAT { Const f }
  | v = variable { Var v }

targetint:
  i = INT { make_targetint i }

tag:
  tag = INT { make_tag ~loc:(make_loc ($startpos, $endpos)) tag }
;

plain_int:
  i = INT { make_plain_int i }
;

field_of_block:
  | s = symbol { Symbol s }
  | v = variable { Dynamically_computed v }
  | i = INT { Tagged_immediate ( make_tagged_immediate ~loc:($startpos, $endpos) i ) }
;

kinded_variable:
  | param = variable; kind = kind_with_subkind_opt { { param; kind } }
;

kind_with_subkind_opt:
  | { None }
  | COLON; kind = kind_with_subkind { Some kind }

simple_args:
  | { [] }
  | LPAREN s = separated_nonempty_list(COMMA, simple); RPAREN { s }
;

const:
  | c = INT { make_const_int c }
  | c = FLOAT { Naked_float c }
;

name:
  | s = symbol { (Symbol s:name) }
  | v = variable { (Var v:name) }
;

func_name_with_optional_arities:
  | n = name { n, None }
  | LPAREN;
      n = name; COLON; params_arity = blank_or(kinds_with_subkinds);
      MINUSGREATER; ret_arity = kinds_with_subkinds;
    RPAREN
    { n, Some ({ params_arity; ret_arity } : function_arities) }
;

blank_or(a):
  | BLANK { None }
  | a = a { Some a }
;

simple:
  | s = symbol { Symbol s }
  | v = variable { Var v }
  | c = const { Const c }
;

code_id:
  | v = variable { v }
;

code_size:
  | i = plain_int { i }

closure_id:
  | v = variable { v }
;

closure_id_opt :
  | { None }
  | AT; cid = closure_id { Some cid }
;

symbol:
  | e = SYMBOL { make_located e ($startpos, $endpos) }
;

variable:
  | e = IDENT { make_located e ($startpos, $endpos) }
;

continuation_id :
  | e = IDENT { make_located e ($startpos, $endpos) }
;

continuation:
  | e = continuation_id { Named e }
  | s = special_continuation { Special s }
;

special_continuation:
  | KWD_DONE { Done }
  | KWD_ERROR { Error }
;

var_within_closure:
  | e = IDENT { make_located e ($startpos, $endpos) }
;
%%
