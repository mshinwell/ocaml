
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNREACHABLE
    | UNDERSCORE
    | UIDENT of (
# 83 "flambda_parser.mly"
       (string)
# 13 "flambda_parser.ml"
  )
    | TAG
    | SWITCH
    | STUB
    | STAR
    | SEMICOLON
    | RPAREN
    | ROOT
    | REC
    | RBRACKET
    | RBRACE
    | PLUSDOT
    | PLUS
    | OPAQUE
    | MUT
    | MINUSGREATER
    | MINUSDOT
    | MINUS
    | LPAREN
    | LIDENT of (
# 64 "flambda_parser.mly"
       (string)
# 36 "flambda_parser.ml"
  )
    | LETK
    | LET
    | LBRACKET
    | LBRACE
    | IS_INT
    | INT of (
# 59 "flambda_parser.mly"
       (string * char option)
# 46 "flambda_parser.ml"
  )
    | IN
    | HCF
    | GET_FIELD
    | FLOAT of (
# 54 "flambda_parser.mly"
       (string * char option)
# 54 "flambda_parser.ml"
  )
    | EXN
    | EQUAL
    | EOF
    | EFFECT
    | DOT
    | DEF
    | CONT
    | COMMA
    | COLONEQUAL
    | COLON
    | CODE
    | CLOSURE
    | CCALL
    | BLOCK
    | BANG
    | AROBASE
    | APPLY
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState202
  | MenhirState198
  | MenhirState195
  | MenhirState191
  | MenhirState187
  | MenhirState185
  | MenhirState182
  | MenhirState180
  | MenhirState179
  | MenhirState178
  | MenhirState177
  | MenhirState175
  | MenhirState173
  | MenhirState170
  | MenhirState169
  | MenhirState167
  | MenhirState164
  | MenhirState162
  | MenhirState161
  | MenhirState160
  | MenhirState156
  | MenhirState154
  | MenhirState148
  | MenhirState145
  | MenhirState139
  | MenhirState136
  | MenhirState133
  | MenhirState125
  | MenhirState123
  | MenhirState120
  | MenhirState117
  | MenhirState115
  | MenhirState113
  | MenhirState112
  | MenhirState110
  | MenhirState109
  | MenhirState104
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState95
  | MenhirState94
  | MenhirState92
  | MenhirState91
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState76
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState65
  | MenhirState63
  | MenhirState56
  | MenhirState53
  | MenhirState49
  | MenhirState43
  | MenhirState40
  | MenhirState33
  | MenhirState29
  | MenhirState26
  | MenhirState23
  | MenhirState22
  | MenhirState20
  | MenhirState19
  | MenhirState17
  | MenhirState12
  | MenhirState9
  | MenhirState8
  | MenhirState5
  | MenhirState1
  | MenhirState0

# 1 "flambda_parser.mly"
  
open Fexpr

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

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

# 200 "flambda_parser.ml"

let rec _menhir_goto_tags_to_sizes : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tags_to_sizes -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv815)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv811)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv809)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_switch_sort = 
# 137 "flambda_parser.mly"
                                                        ( Tag { tags_to_sizes } )
# 225 "flambda_parser.ml"
             in
            _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv810)) : 'freshtv812)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv813)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv814)) : 'freshtv816)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv819 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv817 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)), _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_tags_to_sizes = 
# 134 "flambda_parser.mly"
                                                                  ( tag_to_size :: tags_to_sizes )
# 245 "flambda_parser.ml"
         in
        _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv818)) : 'freshtv820)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_static_structure_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_static_structure_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState195 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv797 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 260 "flambda_parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv795 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 266 "flambda_parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 271 "flambda_parser.ml"
        ))), _, (xs : 'tv_nonempty_list_static_structure_)) = _menhir_stack in
        let _v : 'tv_nonempty_list_static_structure_ = 
# 223 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 276 "flambda_parser.ml"
         in
        _menhir_goto_nonempty_list_static_structure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv796)) : 'freshtv798)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv803 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv799 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | BLOCK ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | CCALL ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState198 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState198 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState198 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198) : 'freshtv800)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv801 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv802)) : 'freshtv804)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv807 * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv805 * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (static : 'tv_nonempty_list_static_structure_)) = _menhir_stack in
        let _v : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 341 "flambda_parser.ml"
        ) = 
# 248 "flambda_parser.mly"
      ( { computation = None; static_structure = static } )
# 345 "flambda_parser.ml"
         in
        _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v) : 'freshtv806)) : 'freshtv808)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv785 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv783 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_variable)), _, (xs : 'tv_list_variable_)) = _menhir_stack in
        let _v : 'tv_list_variable_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 364 "flambda_parser.ml"
         in
        _menhir_goto_list_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv784)) : 'freshtv786)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv793) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv789) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv787) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (v : 'tv_list_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_closure_elements = 
# 270 "flambda_parser.mly"
                                    ( v )
# 385 "flambda_parser.ml"
             in
            _menhir_goto_closure_elements _menhir_env _menhir_stack _v) : 'freshtv788)) : 'freshtv790)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv791) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv792)) : 'freshtv794)
    | _ ->
        _menhir_fail ()

and _menhir_goto_return_kinds : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_kinds -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv777 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_return_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv775 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((k : 'tv_return_kinds) : 'tv_return_kinds) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_return_arity = 
# 184 "flambda_parser.mly"
                           ( Some k )
# 415 "flambda_parser.ml"
         in
        _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v) : 'freshtv776)) : 'freshtv778)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv781 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_return_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv779 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((t : 'tv_return_kinds) : 'tv_return_kinds) = _v in
        ((let (_menhir_stack, _menhir_s, (k : 'tv_kind)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_return_kinds = 
# 180 "flambda_parser.mly"
                                    ( k :: t )
# 432 "flambda_parser.ml"
         in
        _menhir_goto_return_kinds _menhir_env _menhir_stack _menhir_s _v) : 'freshtv780)) : 'freshtv782)
    | _ ->
        _menhir_fail ()

and _menhir_reduce99 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_tags_to_sizes = 
# 132 "flambda_parser.mly"
    ( [] )
# 443 "flambda_parser.ml"
     in
    _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 59 "flambda_parser.mly"
       (string * char option)
# 450 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv771 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 462 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv767 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 472 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 59 "flambda_parser.mly"
       (string * char option)
# 478 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv765 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 486 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos_size_ : Lexing.position) = _endpos in
            let ((size : (
# 59 "flambda_parser.mly"
       (string * char option)
# 492 "flambda_parser.ml"
            )) : (
# 59 "flambda_parser.mly"
       (string * char option)
# 496 "flambda_parser.ml"
            )) = _v in
            let (_startpos_size_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _endpos_tag_, _menhir_s, (tag : (
# 59 "flambda_parser.mly"
       (string * char option)
# 502 "flambda_parser.ml"
            )), _startpos_tag_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_tag_to_size = 
# 125 "flambda_parser.mly"
                               (
  let (tag, _) = tag in
  let (size, _) = size in
  int_of_string tag, int_of_string size
)
# 512 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv763) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_tag_to_size) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv761 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv755 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RBRACKET ->
                    _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv756)
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv757 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)) = _menhir_stack in
                let _v : 'tv_tags_to_sizes = 
# 133 "flambda_parser.mly"
                              ( [ tag_to_size ] )
# 545 "flambda_parser.ml"
                 in
                _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv758)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv759 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv760)) : 'freshtv762)) : 'freshtv764)) : 'freshtv766)) : 'freshtv768)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv769 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 562 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv770)) : 'freshtv772)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv773 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 573 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv774)

and _menhir_goto_switch_sort : _menhir_env -> 'ttv_tail -> 'tv_switch_sort -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv753 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv754)

and _menhir_goto_variable_opt : _menhir_env -> 'ttv_tail -> 'tv_variable_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv751) = Obj.magic _menhir_stack in
    let (_v : 'tv_variable_opt) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv749) = Obj.magic _menhir_stack in
    let ((v : 'tv_variable_opt) : 'tv_variable_opt) = _v in
    ((let _v : 'tv_kinded_variable_opt = 
# 305 "flambda_parser.mly"
                     ( v, None )
# 610 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv747) = _menhir_stack in
    let (_v : 'tv_kinded_variable_opt) = _v in
    ((let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv745 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv741 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | BLOCK ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv742)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv743 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv744)) : 'freshtv746)) : 'freshtv748)) : 'freshtv750)) : 'freshtv752)

and _menhir_goto_list_kinded_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_kinded_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv735 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv731 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv729 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_list_kinded_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_args = 
# 278 "flambda_parser.mly"
                                       ( v )
# 675 "flambda_parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv730)) : 'freshtv732)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv733 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv734)) : 'freshtv736)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv739 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv737 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_kinded_variable)), _, (xs : 'tv_list_kinded_variable_)) = _menhir_stack in
        let _v : 'tv_list_kinded_variable_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 694 "flambda_parser.ml"
         in
        _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv738)) : 'freshtv740)
    | _ ->
        _menhir_fail ()

and _menhir_goto_andk : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_andk -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv725 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv723 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (h : 'tv_continuation_handler)), _, (t : 'tv_andk)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_andk = 
# 243 "flambda_parser.mly"
                                          ( h :: t )
# 714 "flambda_parser.ml"
         in
        _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v) : 'freshtv724)) : 'freshtv726)
    | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv727 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | BLOCK ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | CCALL ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | CLOSURE ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | CONT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
        | HCF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | LETK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | SWITCH ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164) : 'freshtv728)
    | _ ->
        _menhir_fail ()

and _menhir_goto_closure_elements : _menhir_env -> 'ttv_tail -> 'tv_closure_elements -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv721 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MINUSGREATER ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv717 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv718)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv719 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv720)) : 'freshtv722)

and _menhir_goto_list_typed_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_typed_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv707 * _menhir_state * 'tv_typed_variable) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv705 * _menhir_state * 'tv_typed_variable) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_typed_variable)), _, (xs : 'tv_list_typed_variable_)) = _menhir_stack in
        let _v : 'tv_list_typed_variable_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 801 "flambda_parser.ml"
         in
        _menhir_goto_list_typed_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv706)) : 'freshtv708)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv715 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv711 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv709 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_list_typed_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_typed_args = 
# 274 "flambda_parser.mly"
                                      ( v )
# 822 "flambda_parser.ml"
             in
            _menhir_goto_typed_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv710)) : 'freshtv712)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv713 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv714)) : 'freshtv716)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_of_kind_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_of_kind_value_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv689 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 844 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv687 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 850 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 855 "flambda_parser.ml"
        ))), _, (xs : 'tv_list_of_kind_value_)) = _menhir_stack in
        let _v : 'tv_list_of_kind_value_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 860 "flambda_parser.ml"
         in
        _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv688)) : 'freshtv690)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv703 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv699 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv697 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (s : 'tv_symbol)), _, (t : 'tv_tag)), _, (elts : 'tv_list_of_kind_value_)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 883 "flambda_parser.ml"
            ) = 
# 283 "flambda_parser.mly"
    ( ( s, None, Block (t, Immutable, elts) ) )
# 887 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv695) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 895 "flambda_parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv693 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 902 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState195 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DEF | EFFECT | EOF | LET | RBRACE | ROOT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv691 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 914 "flambda_parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 919 "flambda_parser.ml"
                ))) = _menhir_stack in
                let _v : 'tv_nonempty_list_static_structure_ = 
# 221 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [ x ] )
# 924 "flambda_parser.ml"
                 in
                _menhir_goto_nonempty_list_static_structure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv692)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195) : 'freshtv694)) : 'freshtv696)) : 'freshtv698)) : 'freshtv700)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv701 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_simple_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_simple_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv669 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv667 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_simple)), _, (xs : 'tv_list_simple_)) = _menhir_stack in
        let _v : 'tv_list_simple_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 954 "flambda_parser.ml"
         in
        _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv668)) : 'freshtv670)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv677 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv673 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv671 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (s : 'tv_list_simple_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_args = 
# 310 "flambda_parser.mly"
                              ( s )
# 975 "flambda_parser.ml"
             in
            _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv672)) : 'freshtv674)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv675 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv676)) : 'freshtv678)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv685 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv681 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv679 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (t : 'tv_tag)), _, (elts : 'tv_list_simple_)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_named = 
# 161 "flambda_parser.mly"
                                               ( Prim (Block (t, Immutable, elts)) )
# 1004 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv680)) : 'freshtv682)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv683 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv684)) : 'freshtv686)
    | _ ->
        _menhir_fail ()

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_variable_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 1022 "flambda_parser.ml"
     in
    _menhir_goto_list_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce84 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_variable -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
    let _v : 'tv_simple = 
# 325 "flambda_parser.mly"
                 ( Var v )
# 1032 "flambda_parser.ml"
     in
    _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_const -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv665) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_const) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv663) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_const) : 'tv_const) = _v in
    ((let _v : 'tv_simple = 
# 326 "flambda_parser.mly"
              ( Const c )
# 1049 "flambda_parser.ml"
     in
    _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv664)) : 'freshtv666)

and _menhir_goto_return_arity : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_arity -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv649 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv645 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | BLOCK ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CCALL ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv646)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv647 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv648)) : 'freshtv650)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv655 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv651 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | BLOCK ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | CCALL ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv652)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv653 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv654)) : 'freshtv656)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv661 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv657 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112) : 'freshtv658)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv659 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv660)) : 'freshtv662)
    | _ ->
        _menhir_fail ()

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kind = 
# 176 "flambda_parser.mly"
    ( None )
# 1196 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv643) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_kind) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv641 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv637 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL | LBRACE | MINUSGREATER ->
            _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | COMMA ->
            _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv638)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv639 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv640)) : 'freshtv642)) : 'freshtv644)

and _menhir_reduce79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_return_kinds = 
# 179 "flambda_parser.mly"
    ( [] )
# 1235 "flambda_parser.ml"
     in
    _menhir_goto_return_kinds _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exn_and_stub : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exn_and_stub -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv635 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv636)

and _menhir_goto_infix_binop : _menhir_env -> 'ttv_tail -> 'tv_infix_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv633 * _menhir_state * 'tv_simple) * 'tv_infix_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv634)

and _menhir_goto_named : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_named -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState198 | MenhirState175 | MenhirState29 | MenhirState164 | MenhirState73 | MenhirState156 | MenhirState81 | MenhirState145 | MenhirState139 | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv625 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv621 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | BLOCK ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | CCALL ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState139 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState139 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState139 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv622)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv623 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv624)) : 'freshtv626)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv631 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv627 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | BLOCK ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | CCALL ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv628)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv629 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)) : 'freshtv632)
    | _ ->
        _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv619) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 1398 "flambda_parser.ml"
    ) = 
# 188 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
# 1402 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv620)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS_INT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv607) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv605) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : 'tv_switch_sort = 
# 139 "flambda_parser.mly"
           ( Is_int )
# 1422 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv606)) : 'freshtv608)
    | TAG ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv613) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv609) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACKET ->
                _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv610)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv611) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv612)) : 'freshtv614)
    | FLOAT _ | INT _ | LIDENT _ | UIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv615) = Obj.magic _menhir_stack in
        ((let _v : 'tv_switch_sort = 
# 138 "flambda_parser.mly"
    ( Int )
# 1457 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv616)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv617 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv618)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv603) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_unop = 
# 143 "flambda_parser.mly"
           ( Opaque_identity )
# 1478 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv601) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_unop) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv599 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv600)) : 'freshtv602)) : 'freshtv604)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EXN | LIDENT _ | STUB ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 64 "flambda_parser.mly"
       (string)
# 1531 "flambda_parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv587) = Obj.magic _menhir_stack in
        let (_endpos_e_ : Lexing.position) = _endpos in
        let ((e : (
# 64 "flambda_parser.mly"
       (string)
# 1541 "flambda_parser.ml"
        )) : (
# 64 "flambda_parser.mly"
       (string)
# 1545 "flambda_parser.ml"
        )) = _v in
        let (_startpos_e_ : Lexing.position) = _startpos in
        ((let _v : 'tv_variable_opt = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 347 "flambda_parser.mly"
               ( Some (e, make_loc (_startpos, _endpos)) )
# 1553 "flambda_parser.ml"
         in
        _menhir_goto_variable_opt _menhir_env _menhir_stack _v) : 'freshtv588)) : 'freshtv590)
    | MUT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv591 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv592)
    | UNDERSCORE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv595) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv593) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : 'tv_variable_opt = 
# 346 "flambda_parser.mly"
               ( None )
# 1578 "flambda_parser.ml"
         in
        _menhir_goto_variable_opt _menhir_env _menhir_stack _v) : 'freshtv594)) : 'freshtv596)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv597 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv598)

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv585) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 1599 "flambda_parser.ml"
    ) = 
# 187 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 1603 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv581 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv577) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 64 "flambda_parser.mly"
       (string)
# 1652 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv575) = Obj.magic _menhir_stack in
            let (_endpos_s_ : Lexing.position) = _endpos in
            let ((s : (
# 64 "flambda_parser.mly"
       (string)
# 1662 "flambda_parser.ml"
            )) : (
# 64 "flambda_parser.mly"
       (string)
# 1666 "flambda_parser.ml"
            )) = _v in
            let (_startpos_s_ : Lexing.position) = _startpos in
            ((let _v : 'tv_csymbol = let _endpos = _endpos_s_ in
            let _startpos = _startpos_s_ in
            
# 338 "flambda_parser.mly"
               ( s, make_loc (_startpos, _endpos) )
# 1674 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv573) = _menhir_stack in
            let (_v : 'tv_csymbol) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv571 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv567 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAREN ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState109
                | COLON | MINUSGREATER ->
                    _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState109
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv568)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv569 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv570)) : 'freshtv572)) : 'freshtv574)) : 'freshtv576)) : 'freshtv578)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv579 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv580)) : 'freshtv582)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv583 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv584)

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115

and _menhir_run120 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv559 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | BLOCK ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | CCALL ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | CLOSURE ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | CONT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
        | HCF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | LETK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | SWITCH ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175) : 'freshtv560)
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv565 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv561 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182) : 'freshtv562)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv563 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv564)) : 'freshtv566)
    | _ ->
        _menhir_fail ()

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_kinded_variable_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 1824 "flambda_parser.ml"
     in
    _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_definition : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 1831 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv557 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 1840 "flambda_parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv555 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((def : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 1848 "flambda_parser.ml"
    )) : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 1852 "flambda_parser.ml"
    )) = _v in
    ((let ((_menhir_stack, _menhir_s), _, (recu : 'tv_recursive)) = _menhir_stack in
    let _1 = () in
    let _v : 'tv_program_body_elt = 
# 103 "flambda_parser.mly"
                                          ( Define_symbol (recu, def) )
# 1859 "flambda_parser.ml"
     in
    _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv556)) : 'freshtv558)

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_andk = 
# 244 "flambda_parser.mly"
    ( [] )
# 1868 "flambda_parser.ml"
     in
    _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v

and _menhir_run161 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | STUB ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | LIDENT _ ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161

and _menhir_goto_separated_nonempty_list_AND_closure_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_closure_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv549 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | BLOCK ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | CCALL ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | CLOSURE ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | CONT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
        | HCF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LETK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SWITCH ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv550)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv553 * _menhir_state * 'tv_closure)) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv551 * _menhir_state * 'tv_closure)) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_closure)), _, (xs : 'tv_separated_nonempty_list_AND_closure_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_AND_closure_ = 
# 243 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 1943 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_closure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)) : 'freshtv554)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv545 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv543 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (c : 'tv_continuation)), _, (s : 'tv_simple_args)) = _menhir_stack in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 1963 "flambda_parser.ml"
        ) = 
# 189 "flambda_parser.mly"
                                          ( Apply_cont (c, None, s) )
# 1967 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv544)) : 'freshtv546)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv547 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUSGREATER ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv548)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typed_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typed_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv527 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv523 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv524)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv525 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv526)) : 'freshtv528)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv533 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv529 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | BLOCK ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | CCALL ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv530)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv531 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)) : 'freshtv534)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv541 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv535) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACKET ->
                _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv536)
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv537) = Obj.magic _menhir_stack in
            ((let _v : 'tv_closure_elements = 
# 271 "flambda_parser.mly"
    ( [] )
# 2095 "flambda_parser.ml"
             in
            _menhir_goto_closure_elements _menhir_env _menhir_stack _v) : 'freshtv538)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv539 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv540)) : 'freshtv542)
    | _ ->
        _menhir_fail ()

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_typed_variable_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 2113 "flambda_parser.ml"
     in
    _menhir_goto_list_typed_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_SEMICOLON_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv521) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_option_SEMICOLON_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv519) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_option_SEMICOLON_) : 'tv_option_SEMICOLON_) = _v in
    ((let _v : 'tv_switch = 
# 171 "flambda_parser.mly"
                      ( [] )
# 2130 "flambda_parser.ml"
     in
    _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv520)) : 'freshtv522)

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_of_kind_value_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 2139 "flambda_parser.ml"
     in
    _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run188 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 59 "flambda_parser.mly"
       (string * char option)
# 2146 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv517) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 59 "flambda_parser.mly"
       (string * char option)
# 2157 "flambda_parser.ml"
    )) : (
# 59 "flambda_parser.mly"
       (string * char option)
# 2161 "flambda_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2167 "flambda_parser.ml"
    ) = let _endpos = _endpos_i_ in
    let _startpos = _startpos_i_ in
    
# 293 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 2173 "flambda_parser.ml"
     in
    _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv518)

and _menhir_reduce48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_simple_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 2182 "flambda_parser.ml"
     in
    _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 64 "flambda_parser.mly"
       (string)
# 2189 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv515) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 64 "flambda_parser.mly"
       (string)
# 2200 "flambda_parser.ml"
    )) : (
# 64 "flambda_parser.mly"
       (string)
# 2204 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_variable = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 342 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 2212 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv513) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv477 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv475 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (param : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_typed_variable = 
# 297 "flambda_parser.mly"
                     ( { param; ty = () } )
# 2229 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_typed_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv471 * _menhir_state * 'tv_typed_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState12 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv472)) : 'freshtv474)) : 'freshtv476)) : 'freshtv478)
    | MenhirState136 | MenhirState133 | MenhirState125 | MenhirState123 | MenhirState117 | MenhirState86 | MenhirState85 | MenhirState79 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        (_menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) : 'freshtv480)
    | MenhirState173 | MenhirState170 | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv491 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_kinded_variable = 
# 301 "flambda_parser.mly"
                 ( v, None )
# 2262 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv489) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinded_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState76 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv485 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv481 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FLOAT _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | INT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv482)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv483 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv484)) : 'freshtv486)
        | MenhirState173 | MenhirState170 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv487 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173) : 'freshtv488)
        | _ ->
            _menhir_fail ()) : 'freshtv490)) : 'freshtv492)) : 'freshtv494)
    | MenhirState148 | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv495 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LBRACKET | MINUSGREATER ->
            _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv496)
    | MenhirState95 | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv497 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACKET ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv498)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_variable)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_named = 
# 162 "flambda_parser.mly"
                      ( Read_mutable v )
# 2355 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)) : 'freshtv502)
    | MenhirState198 | MenhirState175 | MenhirState29 | MenhirState164 | MenhirState73 | MenhirState156 | MenhirState154 | MenhirState81 | MenhirState145 | MenhirState139 | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv507 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv503 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv504)
        | DOT | IN | MINUS | MINUSDOT | PLUS | PLUSDOT | SEMICOLON ->
            _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv505 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv506)) : 'freshtv508)
    | MenhirState191 | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv511 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv509 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2400 "flambda_parser.ml"
        ) = 
# 292 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 2404 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)) : 'freshtv512)
    | _ ->
        _menhir_fail ()) : 'freshtv514)) : 'freshtv516)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 59 "flambda_parser.mly"
       (string * char option)
# 2413 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv469) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 59 "flambda_parser.mly"
       (string * char option)
# 2424 "flambda_parser.ml"
    )) : (
# 59 "flambda_parser.mly"
       (string * char option)
# 2428 "flambda_parser.ml"
    )) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _v : 'tv_const = 
# 314 "flambda_parser.mly"
            ( make_const_int c )
# 2434 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv470)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 54 "flambda_parser.mly"
       (string * char option)
# 2441 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv467) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 54 "flambda_parser.mly"
       (string * char option)
# 2451 "flambda_parser.ml"
    )) : (
# 54 "flambda_parser.mly"
       (string * char option)
# 2455 "flambda_parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 315 "flambda_parser.mly"
              ( make_const_float c )
# 2460 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv468)

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_return_arity = 
# 183 "flambda_parser.mly"
    ( None )
# 2469 "flambda_parser.ml"
     in
    _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL | LBRACE | MINUSGREATER ->
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | COMMA ->
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_reduce17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_exn_and_stub = 
# 228 "flambda_parser.mly"
    ( false, false )
# 2493 "flambda_parser.ml"
     in
    _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 231 "flambda_parser.mly"
             ( true, true )
# 2515 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)) : 'freshtv462)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv463 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 229 "flambda_parser.mly"
         ( false, true )
# 2526 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv464)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv465 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STUB ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 232 "flambda_parser.mly"
             ( true, true )
# 2555 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)) : 'freshtv454)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv455 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 230 "flambda_parser.mly"
        ( true, false )
# 2566 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv456)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv457 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)

and _menhir_goto_of_kind_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2580 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv449 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2588 "flambda_parser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run188 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState191
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv450)

and _menhir_goto_simple : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv385 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv381 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | RBRACE ->
                _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv382)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv383 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv391 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv387 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | BLOCK ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | CCALL ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv388)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv389 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)) : 'freshtv392)
    | MenhirState117 | MenhirState86 | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv393 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv394)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state * 'tv_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state * 'tv_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (v : 'tv_variable)), _, (s : 'tv_simple)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_named = 
# 163 "flambda_parser.mly"
                                       ( Assign { being_assigned = v; new_value = s } )
# 2722 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)) : 'freshtv398)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv401 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv399 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (u : 'tv_unop)), _, (a : 'tv_simple)) = _menhir_stack in
        let _v : 'tv_named = 
# 158 "flambda_parser.mly"
                        ( Prim (Unop (u, a)) )
# 2734 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)) : 'freshtv402)
    | MenhirState198 | MenhirState175 | MenhirState29 | MenhirState164 | MenhirState73 | MenhirState156 | MenhirState154 | MenhirState81 | MenhirState145 | MenhirState139 | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv429 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv403 * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FLOAT _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
                | INT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv404)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv405 * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
        | MINUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv411) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv409) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 148 "flambda_parser.mly"
          ( Minus )
# 2784 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv410)) : 'freshtv412)
        | MINUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv415) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv413) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 149 "flambda_parser.mly"
             ( Minusdot )
# 2797 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv414)) : 'freshtv416)
        | PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv419) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv417) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 146 "flambda_parser.mly"
         ( Plus )
# 2810 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv418)) : 'freshtv420)
        | PLUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv423) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv421) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 147 "flambda_parser.mly"
            ( Plusdot )
# 2823 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv422)) : 'freshtv424)
        | IN | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv425 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (s : 'tv_simple)) = _menhir_stack in
            let _v : 'tv_named = 
# 157 "flambda_parser.mly"
               ( Simple s )
# 2833 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv427 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)) : 'freshtv430)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv443 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv439 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv437 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a : 'tv_simple)), _, (f : 'tv_simple)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : 'tv_binop = 
# 154 "flambda_parser.mly"
    ( Binop (Block_load (Block Value, Immutable), a, f) )
# 2862 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv435) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_binop) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_binop) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv431) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_binop) : 'tv_binop) = _v in
            ((let _v : 'tv_named = 
# 160 "flambda_parser.mly"
              ( Prim b )
# 2879 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)) : 'freshtv434)) : 'freshtv436)) : 'freshtv438)) : 'freshtv440)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv441 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv442)) : 'freshtv444)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv447 * _menhir_state * 'tv_simple) * 'tv_infix_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv445 * _menhir_state * 'tv_simple) * 'tv_infix_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (a1 : 'tv_simple)), (b : 'tv_infix_binop)), _, (a2 : 'tv_simple)) = _menhir_stack in
        let _v : 'tv_named = 
# 159 "flambda_parser.mly"
                                            ( Prim (Infix_binop (b, a1, a2)) )
# 2898 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program_body_elt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_program_body_elt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_program_body_elt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState202
    | EFFECT ->
        _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState202
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState202
    | ROOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState202
    | EOF ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState202
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202) : 'freshtv380)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_args = 
# 279 "flambda_parser.mly"
    ( [] )
# 2937 "flambda_parser.ml"
     in
    _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run170 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 2959 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv305 * _menhir_state * 'tv_named)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 2969 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * _menhir_state * 'tv_named)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 2975 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (defining_expr : 'tv_named)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 2980 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 2986 "flambda_parser.ml"
        ) = 
# 196 "flambda_parser.mly"
      ( Let { var = None; kind = None; defining_expr; body } )
# 2990 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)) : 'freshtv306)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv323 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 2998 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv319 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3008 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv317 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3015 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _menhir_s, (name : 'tv_variable)), _, (params : 'tv_typed_args)), (closure_vars : 'tv_closure_elements)), _, (ret_cont : 'tv_continuation)), _, (exn_cont : 'tv_option_exn_continuation_)), _, (ret_arity : 'tv_return_arity)), _, (expr : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3020 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _4 = () in
            let _v : 'tv_closure = 
# 225 "flambda_parser.mly"
    ( { name; params; closure_vars; ret_cont; exn_cont; ret_arity; expr } )
# 3028 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv315) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_closure) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv313 * _menhir_state * 'tv_closure) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AND ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv307 * _menhir_state * 'tv_closure) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LIDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv308)
            | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv309 * _menhir_state * 'tv_closure) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_closure)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_AND_closure_ = 
# 241 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [ x ] )
# 3059 "flambda_parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_AND_closure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv311 * _menhir_state * 'tv_closure) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)) : 'freshtv316)) : 'freshtv318)) : 'freshtv320)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv321 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3076 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)) : 'freshtv324)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv327 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3085 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv325 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3091 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (c : 'tv_separated_nonempty_list_AND_closure_)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3096 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3102 "flambda_parser.ml"
        ) = 
# 217 "flambda_parser.mly"
    ( Let_closure { closures = c; body } )
# 3106 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)) : 'freshtv328)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv331 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3114 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv329 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3120 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (v : 'tv_kinded_variable)), _, (initial_value : 'tv_simple)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3125 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3134 "flambda_parser.ml"
        ) = 
# 198 "flambda_parser.mly"
      ( let (var, kind) = v in
        Let_mutable { var; kind; initial_value; body } )
# 3139 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)) : 'freshtv332)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv335 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3147 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv333 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3153 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), (v : 'tv_kinded_variable_opt)), _, (defining_expr : 'tv_named)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3158 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3166 "flambda_parser.ml"
        ) = 
# 193 "flambda_parser.mly"
      ( let (var, kind) = v in
        Let { var; kind; defining_expr; body } )
# 3171 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)) : 'freshtv336)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv349 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3179 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv345 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3189 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv343 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3196 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (exn_and_stub : 'tv_exn_and_stub)), _, (name : 'tv_continuation)), _, (params : 'tv_typed_args)), _, (handler : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3201 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _v : 'tv_continuation_handler = 
# 238 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
# 3209 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv341) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_continuation_handler) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState65 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv337 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160) : 'freshtv338)
            | MenhirState161 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv339 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162) : 'freshtv340)
            | _ ->
                _menhir_fail ()) : 'freshtv342)) : 'freshtv344)) : 'freshtv346)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv347 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3254 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv353 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3263 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv351 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3269 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, (recursive : 'tv_recursive)), _, (handler : 'tv_continuation_handler)), _, (t : 'tv_andk)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3274 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3280 "flambda_parser.ml"
        ) = 
# 201 "flambda_parser.mly"
     ( let handlers = handler :: t in
       Let_cont { recursive; body; handlers } )
# 3285 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)) : 'freshtv354)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv363 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3293 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv361 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3299 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s, (name : 'tv_func_sym)), _, (params : 'tv_typed_args)), _, (ret_cont : 'tv_continuation)), _, (exn_cont : 'tv_option_exn_continuation_)), _, (ret_arity : 'tv_return_arity)), _, (expr : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3304 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _3 = () in
        let _v : 'tv_let_code = 
# 116 "flambda_parser.mly"
  ( ({ name; params; ret_cont; ret_arity; exn_cont; expr } : let_code) )
# 3311 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv359) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_code) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv357 * _menhir_state)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_code) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((let_code : 'tv_let_code) : 'tv_let_code) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 104 "flambda_parser.mly"
                                          ( Let_code let_code )
# 3331 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)) : 'freshtv358)) : 'freshtv360)) : 'freshtv362)) : 'freshtv364)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3339 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv371 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3345 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (c : 'tv_continuation)), _, (v : 'tv_args)), _, (expr : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3350 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_effect = 
# 262 "flambda_parser.mly"
      ( let computation =
          { expr; return_cont = c;
            exception_cont = ("exn", Location.none); computed_values = v }
        in
        { computation = Some computation; static_structure = [] } )
# 3359 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_effect) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv367 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_effect) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv365 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((e : 'tv_effect) : 'tv_effect) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 102 "flambda_parser.mly"
                                          ( Define_symbol (Nonrecursive, e) )
# 3378 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)) : 'freshtv368)) : 'freshtv370)) : 'freshtv372)) : 'freshtv374)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv377 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3386 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv375 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3392 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, (c : 'tv_continuation)), _, (v : 'tv_args)), _, (static : 'tv_nonempty_list_static_structure_)), _, (expr : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3397 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _1 = () in
        let _v : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 3405 "flambda_parser.ml"
        ) = 
# 253 "flambda_parser.mly"
      ( let computation =
          { expr; return_cont = c;
            exception_cont = ("exn", Location.none); computed_values = v }
        in
        { computation = Some computation; static_structure = static } )
# 3413 "flambda_parser.ml"
         in
        _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)) : 'freshtv378)
    | _ ->
        _menhir_fail ()

and _menhir_reduce86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_simple_args = 
# 309 "flambda_parser.mly"
    ( [] )
# 3424 "flambda_parser.ml"
     in
    _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_reduce103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_typed_args = 
# 275 "flambda_parser.mly"
    ( [] )
# 3454 "flambda_parser.ml"
     in
    _menhir_goto_typed_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_goto_switch : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_switch -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv293 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv291 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (h : 'tv_switch_case)), _, (t : 'tv_switch)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch = 
# 173 "flambda_parser.mly"
                                         ( h :: t )
# 3487 "flambda_parser.ml"
         in
        _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)) : 'freshtv294)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv301 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv297 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv295 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (sort : 'tv_switch_sort)), _, (scrutinee : 'tv_simple)), _, (cases : 'tv_switch)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3509 "flambda_parser.ml"
            ) = 
# 191 "flambda_parser.mly"
    ( Switch {scrutinee; sort; cases} )
# 3513 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv299 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)
    | _ ->
        _menhir_fail ()

and _menhir_reduce66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_SEMICOLON_ = 
# 114 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( None )
# 3531 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let x = () in
    let _v : 'tv_option_SEMICOLON_ = 
# 116 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( Some x )
# 3545 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 59 "flambda_parser.mly"
       (string * char option)
# 3552 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
    let (_endpos_tag_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((tag : (
# 59 "flambda_parser.mly"
       (string * char option)
# 3563 "flambda_parser.ml"
    )) : (
# 59 "flambda_parser.mly"
       (string * char option)
# 3567 "flambda_parser.ml"
    )) = _v in
    let (_startpos_tag_ : Lexing.position) = _startpos in
    ((let _v : 'tv_tag = let _endpos = _endpos_tag_ in
    let _startpos = _startpos_tag_ in
    
# 287 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
# 3575 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv285) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_tag) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv271 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv268)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv269 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)) : 'freshtv272)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv277 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv273 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv275 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv283 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv279 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run188 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv281 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | _ ->
        _menhir_fail ()) : 'freshtv286)) : 'freshtv288)

and _menhir_goto_option_exn_continuation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_exn_continuation_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv263 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | EQUAL ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv264)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv265 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LBRACE ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv266)
    | _ ->
        _menhir_fail ()

and _menhir_reduce68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_exn_continuation_ = 
# 114 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( None )
# 3715 "flambda_parser.ml"
     in
    _menhir_goto_option_exn_continuation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_goto_recursive : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_recursive -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EXN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | STUB ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LIDENT _ ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv258)
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LETK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv259) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState178 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState179 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179) : 'freshtv260)
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv262)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_program_body_elt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_program_body_elt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState202 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv241 * _menhir_state * 'tv_program_body_elt) * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv239 * _menhir_state * 'tv_program_body_elt) * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_program_body_elt)), _, (xs : 'tv_list_program_body_elt_)) = _menhir_stack in
        let _v : 'tv_list_program_body_elt_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 3794 "flambda_parser.ml"
         in
        _menhir_goto_list_program_body_elt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)) : 'freshtv242)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (elts : 'tv_list_program_body_elt_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3813 "flambda_parser.ml"
            ) = 
# 98 "flambda_parser.mly"
                                       ( elts )
# 3817 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3825 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3833 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3841 "flambda_parser.ml"
            )) : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3845 "flambda_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv244)) : 'freshtv246)) : 'freshtv248)) : 'freshtv250)) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 83 "flambda_parser.mly"
       (string)
# 3861 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv237) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 83 "flambda_parser.mly"
       (string)
# 3872 "flambda_parser.ml"
    )) : (
# 83 "flambda_parser.mly"
       (string)
# 3876 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_symbol = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 334 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 3884 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv235) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_symbol) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (s : 'tv_symbol)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 105 "flambda_parser.mly"
                                          ( Root s )
# 3902 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)) : 'freshtv208)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_func_sym = 
# 330 "flambda_parser.mly"
               ( s )
# 3914 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_sym) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_func_sym) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | MINUSGREATER ->
            _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)
    | MenhirState198 | MenhirState175 | MenhirState29 | MenhirState164 | MenhirState73 | MenhirState156 | MenhirState154 | MenhirState81 | MenhirState145 | MenhirState139 | MenhirState136 | MenhirState133 | MenhirState104 | MenhirState125 | MenhirState123 | MenhirState117 | MenhirState86 | MenhirState85 | MenhirState79 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_simple = 
# 324 "flambda_parser.mly"
               ( Symbol s )
# 3943 "flambda_parser.ml"
         in
        _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)
    | MenhirState178 | MenhirState195 | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BLOCK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv221 * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185) : 'freshtv222)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv223 * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | MenhirState191 | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3993 "flambda_parser.ml"
        ) = 
# 291 "flambda_parser.mly"
               ( Symbol s )
# 3997 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
    | _ ->
        _menhir_fail ()) : 'freshtv236)) : 'freshtv238)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 64 "flambda_parser.mly"
       (string)
# 4006 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 64 "flambda_parser.mly"
       (string)
# 4017 "flambda_parser.ml"
    )) : (
# 64 "flambda_parser.mly"
       (string)
# 4021 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_continuation = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 351 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 4029 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv201) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_continuation) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv159 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | COLON | EQUAL ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv160)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (cont : 'tv_continuation)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_continuation = 
# 109 "flambda_parser.mly"
                             ( cont )
# 4061 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_exn_continuation) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_exn_continuation) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_exn_continuation) : 'tv_exn_continuation) = _v in
        ((let _v : 'tv_option_exn_continuation_ = 
# 116 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( Some x )
# 4078 "flambda_parser.ml"
         in
        _menhir_goto_option_exn_continuation_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)) : 'freshtv166)) : 'freshtv168)) : 'freshtv170)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv183 * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181 * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : 'tv_tag)), _, (c : 'tv_continuation)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch_case = 
# 167 "flambda_parser.mly"
                                          ( i,c )
# 4091 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_switch_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | RBRACE ->
                _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv172)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (c : 'tv_switch_case)) = _menhir_stack in
            let _v : 'tv_switch = 
# 172 "flambda_parser.mly"
                    ( [c] )
# 4126 "flambda_parser.ml"
             in
            _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv175 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)) : 'freshtv182)) : 'freshtv184)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LBRACE ->
            _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv186)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEF | EFFECT | EOF | LET | RBRACE | ROOT ->
            _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv188)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv189 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | COLON | LBRACE ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv190)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv191 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv192)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv195 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv193 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s), (func : 'tv_csymbol)), _, (args : 'tv_simple_args)), _, (ra : 'tv_return_arity)), _, (r : 'tv_continuation)), _, (e : 'tv_continuation)) = _menhir_stack in
        let _7 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 4203 "flambda_parser.ml"
        ) = 
# 205 "flambda_parser.mly"
     ( Apply {
          func = Symbol func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = C_call {
              alloc = true; (* TODO noalloc *)
              (* param_arity =; *)
              return_arity = ra;
            };
       })
# 4217 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)) : 'freshtv196)
    | MenhirState167 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv198)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv199 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | LBRACE ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180) : 'freshtv200)
    | _ ->
        _menhir_fail ()) : 'freshtv202)) : 'freshtv204)

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_recursive = 
# 120 "flambda_parser.mly"
    ( Nonrecursive )
# 4256 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_recursive = 
# 121 "flambda_parser.mly"
        ( Recursive )
# 4270 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState202 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * 'tv_program_body_elt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv9 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState195 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 4292 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4301 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv15 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv17 * _menhir_state * 'tv_symbol))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState167 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv39 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv47 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state) * 'tv_kinded_variable_opt)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_closure)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * 'tv_named)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_simple) * 'tv_infix_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv59 * _menhir_state * 'tv_simple))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_variable)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv71 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv73 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv75 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv77 * _menhir_state)) * 'tv_csymbol)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv79 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv81 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv83 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv85 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv90)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv103 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv105 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv109 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state * 'tv_switch_case)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv123 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state * 'tv_tag_to_size)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv130)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv131 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv137 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv141 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv143 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_typed_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_func_sym) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv156)

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_program_body_elt_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 4663 "flambda_parser.ml"
     in
    _menhir_goto_list_program_body_elt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CODE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)

and _menhir_run167 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167

and _menhir_run177 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LETK | UIDENT _ ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 4749 "flambda_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EFFECT ->
        _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ROOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 353 "flambda_parser.mly"
  

# 4786 "flambda_parser.ml"

# 269 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
  

# 4791 "flambda_parser.ml"
