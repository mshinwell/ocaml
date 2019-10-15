%{
open Fexpr

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    failwith "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, Some 't' -> s
  | _, _ ->
    failwith "Tagged immediates must have modifier 't'"

let make_const_int (i, m) : Fexpr.const =
  match m with
  | None -> Naked_nativeint (Int64.of_string i)
  | Some 'u' -> Naked_immediate i
  | Some 't' -> Tagged_immediate i
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown int modifier %c" c)

let make_const_float (i, m) =
  match m with
  | None -> Naked_float (float_of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown float modifier %c" c)
%}

/* Tokens */

%token AND   [@symbol "and"]
%token APPLY [@symbol "apply"]
%token AROBASE [@symbol "@"]
%token BANG [@symbol "!"]
%token BLOCK [@symbol "Block"]
%token CCALL  [@symbol "ccall"]
%token CODE  [@symbol "code"]
%token COLON  [@symbol ":"]
%token COLONEQUAL  [@symbol ":="]
%token COMMA  [@symbol ","]
%token CONT  [@symbol "cont"]
%token DEF   [@symbol "def"]
%token EFFECT [@symbol "effect"]
%token EQUAL [@symbol "="]
%token EXN   [@symbol "exn"]
%token <string * char option> FLOAT
%token HCF   [@symbol "HCF"]
%token IN    [@symbol "in"]
%token <string * char option> INT
%token LBRACE [@symbol "{"]
%token LBRACKET [@symbol "["]
%token LET    [@symbol "let"]
%token LETK   [@symbol "letk"]
%token <string> LIDENT
%token LPAREN [@symbol "("]
%token MINUS    [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token MUT    [@symbol "mut"]
%token OPAQUE [@symbol "opaque_identity"]
%token PLUS     [@symbol "+"]
%token PLUSDOT  [@symbol "+."]
%token RBRACE [@symbol "}"]
%token RBRACKET [@symbol "]"]
%token REC    [@symbol "rec"]
%token RPAREN [@symbol ")"]
%token ROOT   [@symbol "root"]
%token SEMICOLON [@symbol ";"]
%token STUB   [@symbol "stub"]
%token STAR   [@symbol "*"]
%token SWITCH [@symbol "switch"]
%token TAG    [@symbol "tag"]
%token <string> UIDENT
%token UNDERSCORE [@symbol "_"]
%token UNREACHABLE [@symbol "Unreachable"]
%token EOF

%start program
%type <Fexpr.program> program
%type <Fexpr.definition> definition
%type <Fexpr.static_structure> static_structure
%type <Fexpr.expr> expr
%type <Fexpr.name> name
%type <Fexpr.of_kind_value> of_kind_value
%%

program:
  | elts = program_body_elt* EOF       { elts }
;

program_body_elt:
  | EFFECT e = effect                     { Define_symbol (Nonrecursive, e) }
  | DEF recu = recursive def = definition { Define_symbol (recu, def) }
  | LET CODE let_code = let_code          { Let_code let_code }
  | ROOT s = symbol                       { Root s }
;

exn_continuation:
  | STAR cont = continuation { cont }

let_code:
  | name = func_sym params = typed_args
  MINUSGREATER ret_cont = continuation
  exn_cont = option(exn_continuation)
  ret_arity = return_arity EQUAL expr = expr
  { ({ name; params; ret_cont; ret_arity; exn_cont; expr } : let_code) }
;

recursive:
  | { Nonrecursive }
  | REC { Recursive }
;

is_tag:
  | TAG { Fabricated }
  | { Value }
;

unop:
  | OPAQUE { Opaque_identity }

binop:
  | PLUS { Plus }
  | PLUSDOT { Plusdot }
  | MINUS { Minus }
  | MINUSDOT { Minusdot }
;

named:
  | s = simple { Simple s }
  | u = unop a = simple { Prim (Unop (u, a)) }
  | a1 = simple b = binop a2 = simple { Prim (Binop (b, a1, a2)) }
  | BLOCK t = tag LPAREN elts = simple* RPAREN { Prim (Block (t, Immutable, elts)) }
  | BANG v = variable { Read_mutable v }
  | v = variable COLONEQUAL s = simple { Assign { being_assigned = v; new_value = s } }
;

switch_case:
  | i = tag MINUSGREATER c = continuation { i,c }
;

switch:
  | option(SEMICOLON) { [] }
  | c = switch_case { [c] }
  | h = switch_case SEMICOLON t = switch { h :: t }
;
kind:
  | { () }
;
return_kinds:
  | { [] }
  | k = kind COMMA t = return_kinds { k :: t }
;
return_arity:
  | { None }
  | COLON k = return_kinds { Some k }
;
expr:
  | HCF { Invalid Halt_and_catch_fire }
  | UNREACHABLE { Invalid Treat_as_unreachable }
  | CONT c = continuation s = simple_args { Apply_cont (c, None, s) }
  | SWITCH f = is_tag scrutinee = name LBRACE cases = switch RBRACE
    { Switch {scrutinee; is_fabricated = f; cases} }
  | LET v = kinded_variable_opt EQUAL defining_expr = named IN body = expr
      { let (var, kind) = v in
        Let { var; kind; defining_expr; body } }
  | defining_expr = named SEMICOLON body = expr
      { Let { var = None; kind = (); defining_expr; body } }
  | LET MUT v = kinded_variable EQUAL initial_value = simple IN body = expr
      { let (var, kind) = v in
        Let_mutable { var; kind; initial_value; body } }
  | LETK recursive = recursive handler = continuation_handler t = andk body = expr
     { let handlers = handler :: t in
       Let_cont { recursive; body; handlers } }
  | CCALL LBRACKET func = csymbol RBRACKET args = simple_args ra = return_arity
    MINUSGREATER r = continuation e = continuation
     { Apply {
          func = Symbol func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = C_call {
              alloc = true; (* TODO noalloc *)
              (* param_arity =; *)
              return_arity = ra;
            };
       }}
;

exn_and_stub:
  | { false, false }
  | STUB { false, true }
  | EXN { true, false }
  | STUB EXN { true, true }
  | EXN STUB { true, true }
;

continuation_handler:
  | exn_and_stub = exn_and_stub name = continuation
    params = typed_args LBRACE handler = expr RBRACE
    { let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } }
;

andk:
  | AND h = continuation_handler t = andk { h :: t }
  | { [] }

definition:
  | static = static_structure+
      { { computation = None; static_structure = static } }
  | LETK c = continuation v = args LBRACE
         static = static_structure+
      RBRACE
      expr = expr
      { let computation =
          { expr; return_cont = c;
            exception_cont = ("exn", Loc.none); computed_values = v }
        in
        { computation = Some computation; static_structure = static } }
;

effect:
  | c = continuation v = args expr = expr
      { let computation =
          { expr; return_cont = c;
            exception_cont = ("exn", Loc.none); computed_values = v }
        in
        { computation = Some computation; static_structure = [] } }
;

typed_args:
  | LPAREN v = typed_variable* RPAREN { v }
  | { [] }

args:
  | LPAREN v = kinded_variable* RPAREN { v }
  | { [] }

static_structure:
  | s = symbol EQUAL BLOCK t = tag LPAREN elts = of_kind_value* RPAREN
    { ( s, (), Block (t, Immutable, elts) ) }
;

tag:
  tag = INT { make_tag ~loc:($startpos, $endpos) tag }
;

of_kind_value:
  | s = symbol { Symbol s }
  | v = variable { Dynamically_computed v }
  | i = INT { Tagged_immediate ( make_tagged_immediate ~loc:($startpos, $endpos) i ) }
;

typed_variable:
  | param = variable { { param; ty = () } }
;

kinded_variable:
  | v = variable { v, () }
;

kinded_variable_opt:
  | v = variable_opt { v, () }
;

simple_args:
  | { [] }
  | LPAREN s = simple* RPAREN { s }
;

const:
  | c = INT { make_const_int c }
  | c = FLOAT { make_const_float c }
;

name:
  | s = symbol { Symbol s }
  | v = variable { Var v }
;

simple:
  | s = symbol { Symbol s }
  | v = variable { Var v }
  | c = const { Const c }
;

func_sym:
  | s = symbol { s }
;

symbol:
  | e = UIDENT { e, ($startpos, $endpos) }
;

csymbol:
  | s = LIDENT { s, ($startpos, $endpos) }
;

variable:
  | e = LIDENT { e, ($startpos, $endpos) }
;

variable_opt:
  | UNDERSCORE { None }
  | e = LIDENT { Some (e, ($startpos, $endpos)) }
;

continuation:
  | e = LIDENT { e, ($startpos, $endpos) }
;
%%
