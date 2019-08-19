
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNREACHABLE
    | UNDERSCORE
    | UIDENT of (
# 80 "flambda_parser.mly"
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
# 61 "flambda_parser.mly"
       (string)
# 36 "flambda_parser.ml"
  )
    | LETK
    | LET
    | LBRACKET
    | LBRACE
    | IS_INT
    | INT of (
# 56 "flambda_parser.mly"
       (string * char option)
# 46 "flambda_parser.ml"
  )
    | IN
    | HCF
    | FLOAT of (
# 52 "flambda_parser.mly"
       (string * char option)
# 53 "flambda_parser.ml"
  )
    | EXN
    | EQUAL
    | EOF
    | EFFECT
    | DEF
    | CONT
    | COMMA
    | COLONEQUAL
    | COLON
    | CODE
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
  | MenhirState176
  | MenhirState172
  | MenhirState169
  | MenhirState165
  | MenhirState161
  | MenhirState159
  | MenhirState156
  | MenhirState154
  | MenhirState153
  | MenhirState152
  | MenhirState151
  | MenhirState149
  | MenhirState147
  | MenhirState144
  | MenhirState143
  | MenhirState141
  | MenhirState137
  | MenhirState135
  | MenhirState134
  | MenhirState133
  | MenhirState129
  | MenhirState127
  | MenhirState121
  | MenhirState118
  | MenhirState111
  | MenhirState109
  | MenhirState106
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState98
  | MenhirState96
  | MenhirState95
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

# 185 "flambda_parser.ml"

let rec _menhir_goto_tags_to_sizes : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tags_to_sizes -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv709)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv705)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv703)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_switch_sort = 
# 134 "flambda_parser.mly"
                                                        ( Tag { tags_to_sizes } )
# 210 "flambda_parser.ml"
             in
            _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv704)) : 'freshtv706)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv707)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv708)) : 'freshtv710)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv713 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv711 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)), _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_tags_to_sizes = 
# 131 "flambda_parser.mly"
                                                                  ( tag_to_size :: tags_to_sizes )
# 230 "flambda_parser.ml"
         in
        _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv712)) : 'freshtv714)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_static_structure_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_static_structure_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv691 * _menhir_state * (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 245 "flambda_parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv689 * _menhir_state * (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 251 "flambda_parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 256 "flambda_parser.ml"
        ))), _, (xs : 'tv_nonempty_list_static_structure_)) = _menhir_stack in
        let _v : 'tv_nonempty_list_static_structure_ = 
# 223 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( x :: xs )
# 261 "flambda_parser.ml"
         in
        _menhir_goto_nonempty_list_static_structure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv690)) : 'freshtv692)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv697 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv693 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | BLOCK ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | CCALL ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172) : 'freshtv694)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv695 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv696)) : 'freshtv698)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv701 * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv699 * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (static : 'tv_nonempty_list_static_structure_)) = _menhir_stack in
        let _v : (
# 87 "flambda_parser.mly"
      (Fexpr.definition)
# 324 "flambda_parser.ml"
        ) = 
# 231 "flambda_parser.mly"
      ( { computation = None; static_structure = static } )
# 328 "flambda_parser.ml"
         in
        _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v) : 'freshtv700)) : 'freshtv702)
    | _ ->
        _menhir_fail ()

and _menhir_goto_return_kinds : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_kinds -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv683 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_return_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv681 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((k : 'tv_return_kinds) : 'tv_return_kinds) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_return_arity = 
# 176 "flambda_parser.mly"
                           ( Some k )
# 351 "flambda_parser.ml"
         in
        _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v) : 'freshtv682)) : 'freshtv684)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv687 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_return_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv685 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((t : 'tv_return_kinds) : 'tv_return_kinds) = _v in
        ((let (_menhir_stack, _menhir_s, (k : 'tv_kind)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_return_kinds = 
# 172 "flambda_parser.mly"
                                    ( k :: t )
# 368 "flambda_parser.ml"
         in
        _menhir_goto_return_kinds _menhir_env _menhir_stack _menhir_s _v) : 'freshtv686)) : 'freshtv688)
    | _ ->
        _menhir_fail ()

and _menhir_reduce89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_tags_to_sizes = 
# 129 "flambda_parser.mly"
    ( [] )
# 379 "flambda_parser.ml"
     in
    _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 56 "flambda_parser.mly"
       (string * char option)
# 386 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv677 * Lexing.position * _menhir_state * (
# 56 "flambda_parser.mly"
       (string * char option)
# 398 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv673 * Lexing.position * _menhir_state * (
# 56 "flambda_parser.mly"
       (string * char option)
# 408 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 56 "flambda_parser.mly"
       (string * char option)
# 414 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv671 * Lexing.position * _menhir_state * (
# 56 "flambda_parser.mly"
       (string * char option)
# 422 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos_size_ : Lexing.position) = _endpos in
            let ((size : (
# 56 "flambda_parser.mly"
       (string * char option)
# 428 "flambda_parser.ml"
            )) : (
# 56 "flambda_parser.mly"
       (string * char option)
# 432 "flambda_parser.ml"
            )) = _v in
            let (_startpos_size_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _endpos_tag_, _menhir_s, (tag : (
# 56 "flambda_parser.mly"
       (string * char option)
# 438 "flambda_parser.ml"
            )), _startpos_tag_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_tag_to_size = 
# 122 "flambda_parser.mly"
                               (
  let (tag, _) = tag in
  let (size, _) = size in
  int_of_string tag, int_of_string size
)
# 448 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv669) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_tag_to_size) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv667 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv661 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RBRACKET ->
                    _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv662)
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv663 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)) = _menhir_stack in
                let _v : 'tv_tags_to_sizes = 
# 130 "flambda_parser.mly"
                              ( [ tag_to_size ] )
# 481 "flambda_parser.ml"
                 in
                _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv664)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv665 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv666)) : 'freshtv668)) : 'freshtv670)) : 'freshtv672)) : 'freshtv674)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv675 * Lexing.position * _menhir_state * (
# 56 "flambda_parser.mly"
       (string * char option)
# 498 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv676)) : 'freshtv678)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv679 * Lexing.position * _menhir_state * (
# 56 "flambda_parser.mly"
       (string * char option)
# 509 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv680)

and _menhir_goto_switch_sort : _menhir_env -> 'ttv_tail -> 'tv_switch_sort -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv659 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv660)

and _menhir_goto_variable_opt : _menhir_env -> 'ttv_tail -> 'tv_variable_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv657) = Obj.magic _menhir_stack in
    let (_v : 'tv_variable_opt) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv655) = Obj.magic _menhir_stack in
    let ((v : 'tv_variable_opt) : 'tv_variable_opt) = _v in
    ((let _v : 'tv_kinded_variable_opt = 
# 284 "flambda_parser.mly"
                     ( v, None )
# 546 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv653) = _menhir_stack in
    let (_v : 'tv_kinded_variable_opt) = _v in
    ((let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv651 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv647 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | BLOCK ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv648)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv649 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv650)) : 'freshtv652)) : 'freshtv654)) : 'freshtv656)) : 'freshtv658)

and _menhir_goto_list_kinded_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_kinded_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv641 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv637 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv635 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_list_kinded_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_args = 
# 257 "flambda_parser.mly"
                                       ( v )
# 611 "flambda_parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv636)) : 'freshtv638)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv639 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv640)) : 'freshtv642)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv645 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv643 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_kinded_variable)), _, (xs : 'tv_list_kinded_variable_)) = _menhir_stack in
        let _v : 'tv_list_kinded_variable_ = 
# 213 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( x :: xs )
# 630 "flambda_parser.ml"
         in
        _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv644)) : 'freshtv646)
    | _ ->
        _menhir_fail ()

and _menhir_goto_andk : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_andk -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv631 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv629 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (h : 'tv_continuation_handler)), _, (t : 'tv_andk)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_andk = 
# 226 "flambda_parser.mly"
                                          ( h :: t )
# 650 "flambda_parser.ml"
         in
        _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v) : 'freshtv630)) : 'freshtv632)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv633 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | BLOCK ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | CCALL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | CONT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
        | HCF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | LETK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | SWITCH ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137) : 'freshtv634)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_typed_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_typed_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * _menhir_state * 'tv_typed_variable) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv617 * _menhir_state * 'tv_typed_variable) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_typed_variable)), _, (xs : 'tv_list_typed_variable_)) = _menhir_stack in
        let _v : 'tv_list_typed_variable_ = 
# 213 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( x :: xs )
# 707 "flambda_parser.ml"
         in
        _menhir_goto_list_typed_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv618)) : 'freshtv620)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv627 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv623 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv621 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_list_typed_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_typed_args = 
# 253 "flambda_parser.mly"
                                      ( v )
# 728 "flambda_parser.ml"
             in
            _menhir_goto_typed_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv622)) : 'freshtv624)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv625 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv626)) : 'freshtv628)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_of_kind_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_of_kind_value_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv601 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 750 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv599 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 756 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 761 "flambda_parser.ml"
        ))), _, (xs : 'tv_list_of_kind_value_)) = _menhir_stack in
        let _v : 'tv_list_of_kind_value_ = 
# 213 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( x :: xs )
# 766 "flambda_parser.ml"
         in
        _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv600)) : 'freshtv602)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv615 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv611 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv609 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (s : 'tv_symbol)), _, (t : 'tv_tag)), _, (elts : 'tv_list_of_kind_value_)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 789 "flambda_parser.ml"
            ) = 
# 262 "flambda_parser.mly"
    ( ( s, None, Block (t, Immutable, elts) ) )
# 793 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv607) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 801 "flambda_parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv605 * _menhir_state * (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 808 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DEF | EFFECT | EOF | LET | RBRACE | ROOT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv603 * _menhir_state * (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 820 "flambda_parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 825 "flambda_parser.ml"
                ))) = _menhir_stack in
                let _v : 'tv_nonempty_list_static_structure_ = 
# 221 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( [ x ] )
# 830 "flambda_parser.ml"
                 in
                _menhir_goto_nonempty_list_static_structure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv606)) : 'freshtv608)) : 'freshtv610)) : 'freshtv612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv613 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv614)) : 'freshtv616)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_simple_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_simple_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv581 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv579 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_simple)), _, (xs : 'tv_list_simple_)) = _menhir_stack in
        let _v : 'tv_list_simple_ = 
# 213 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( x :: xs )
# 860 "flambda_parser.ml"
         in
        _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv580)) : 'freshtv582)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv589 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv585 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv583 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (s : 'tv_list_simple_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_args = 
# 289 "flambda_parser.mly"
                              ( s )
# 881 "flambda_parser.ml"
             in
            _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv584)) : 'freshtv586)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv587 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv588)) : 'freshtv590)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv597 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv593 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv591 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (t : 'tv_tag)), _, (elts : 'tv_list_simple_)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_named = 
# 153 "flambda_parser.mly"
                                               ( Prim (Block (t, Immutable, elts)) )
# 910 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)) : 'freshtv594)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv595 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv596)) : 'freshtv598)
    | _ ->
        _menhir_fail ()

and _menhir_reduce74 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_variable -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
    let _v : 'tv_simple = 
# 304 "flambda_parser.mly"
                 ( Var v )
# 929 "flambda_parser.ml"
     in
    _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_const -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv577) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_const) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv575) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_const) : 'tv_const) = _v in
    ((let _v : 'tv_simple = 
# 305 "flambda_parser.mly"
              ( Const c )
# 946 "flambda_parser.ml"
     in
    _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv576)) : 'freshtv578)

and _menhir_goto_return_arity : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_arity -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv567 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv563 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | BLOCK ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CCALL ->
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv564)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv565 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv573 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv569 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv570)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv571 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv572)) : 'freshtv574)
    | _ ->
        _menhir_fail ()

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kind = 
# 168 "flambda_parser.mly"
    ( None )
# 1038 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv561) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_kind) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv555 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL | MINUSGREATER ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | COMMA ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv556)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv558)) : 'freshtv560)) : 'freshtv562)

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_return_kinds = 
# 171 "flambda_parser.mly"
    ( [] )
# 1077 "flambda_parser.ml"
     in
    _menhir_goto_return_kinds _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exn_and_stub : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exn_and_stub -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv553 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv554)

and _menhir_goto_binop : _menhir_env -> 'ttv_tail -> 'tv_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv551 * _menhir_state * 'tv_simple) * 'tv_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv552)

and _menhir_goto_named : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_named -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState172 | MenhirState149 | MenhirState29 | MenhirState137 | MenhirState73 | MenhirState129 | MenhirState121 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv543 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv539 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | BLOCK ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | CCALL ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv540)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv541 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv542)) : 'freshtv544)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv549 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv545 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | BLOCK ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | CCALL ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv546)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv547 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv548)) : 'freshtv550)
    | _ ->
        _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv537) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 1236 "flambda_parser.ml"
    ) = 
# 180 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
# 1240 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv538)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS_INT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv525) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv523) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : 'tv_switch_sort = 
# 136 "flambda_parser.mly"
           ( Is_int )
# 1260 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv524)) : 'freshtv526)
    | TAG ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv531) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv527) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACKET ->
                _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv528)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv529) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv530)) : 'freshtv532)
    | FLOAT _ | INT _ | LIDENT _ | UIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv533) = Obj.magic _menhir_stack in
        ((let _v : 'tv_switch_sort = 
# 135 "flambda_parser.mly"
    ( Int )
# 1295 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv534)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv535 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv536)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv521) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_unop = 
# 140 "flambda_parser.mly"
           ( Opaque_identity )
# 1316 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv519) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_unop) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv517 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv518)) : 'freshtv520)) : 'freshtv522)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EXN | LIDENT _ | STUB ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState63
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
        let (_menhir_stack : 'freshtv507) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 61 "flambda_parser.mly"
       (string)
# 1369 "flambda_parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv505) = Obj.magic _menhir_stack in
        let (_endpos_e_ : Lexing.position) = _endpos in
        let ((e : (
# 61 "flambda_parser.mly"
       (string)
# 1379 "flambda_parser.ml"
        )) : (
# 61 "flambda_parser.mly"
       (string)
# 1383 "flambda_parser.ml"
        )) = _v in
        let (_startpos_e_ : Lexing.position) = _startpos in
        ((let _v : 'tv_variable_opt = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 326 "flambda_parser.mly"
               ( Some (e, make_loc (_startpos, _endpos)) )
# 1391 "flambda_parser.ml"
         in
        _menhir_goto_variable_opt _menhir_env _menhir_stack _v) : 'freshtv506)) : 'freshtv508)
    | MUT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv509 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv510)
    | UNDERSCORE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv513) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv511) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : 'tv_variable_opt = 
# 325 "flambda_parser.mly"
               ( None )
# 1416 "flambda_parser.ml"
         in
        _menhir_goto_variable_opt _menhir_env _menhir_stack _v) : 'freshtv512)) : 'freshtv514)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv515 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv516)

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv503) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 1437 "flambda_parser.ml"
    ) = 
# 179 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 1441 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)

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
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv499 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv495) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 61 "flambda_parser.mly"
       (string)
# 1477 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv493) = Obj.magic _menhir_stack in
            let (_endpos_s_ : Lexing.position) = _endpos in
            let ((s : (
# 61 "flambda_parser.mly"
       (string)
# 1487 "flambda_parser.ml"
            )) : (
# 61 "flambda_parser.mly"
       (string)
# 1491 "flambda_parser.ml"
            )) = _v in
            let (_startpos_s_ : Lexing.position) = _startpos in
            ((let _v : 'tv_csymbol = let _endpos = _endpos_s_ in
            let _startpos = _startpos_s_ in
            
# 317 "flambda_parser.mly"
               ( s, make_loc (_startpos, _endpos) )
# 1499 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv491) = _menhir_stack in
            let (_v : 'tv_csymbol) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv489 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv485 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAREN ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | COLON | MINUSGREATER ->
                    _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv486)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv487 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)) : 'freshtv490)) : 'freshtv492)) : 'freshtv494)) : 'freshtv496)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv497 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv498)) : 'freshtv500)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv501 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)

and _menhir_run101 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv477 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | BLOCK ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | CCALL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | CONT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | HCF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LETK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SWITCH ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv478)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv483 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv479 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv480)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv481 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)) : 'freshtv484)
    | _ ->
        _menhir_fail ()

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_kinded_variable_ = 
# 211 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( [] )
# 1647 "flambda_parser.ml"
     in
    _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_definition : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 87 "flambda_parser.mly"
      (Fexpr.definition)
# 1654 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 87 "flambda_parser.mly"
      (Fexpr.definition)
# 1663 "flambda_parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv473 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((def : (
# 87 "flambda_parser.mly"
      (Fexpr.definition)
# 1671 "flambda_parser.ml"
    )) : (
# 87 "flambda_parser.mly"
      (Fexpr.definition)
# 1675 "flambda_parser.ml"
    )) = _v in
    ((let ((_menhir_stack, _menhir_s), _, (recu : 'tv_recursive)) = _menhir_stack in
    let _1 = () in
    let _v : 'tv_program_body_elt = 
# 100 "flambda_parser.mly"
                                          ( Define_symbol (recu, def) )
# 1682 "flambda_parser.ml"
     in
    _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv474)) : 'freshtv476)

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_andk = 
# 227 "flambda_parser.mly"
    ( [] )
# 1691 "flambda_parser.ml"
     in
    _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | STUB ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | LIDENT _ ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134

and _menhir_goto_simple_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv469 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv467 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (c : 'tv_continuation)), _, (s : 'tv_simple_args)) = _menhir_stack in
        let _1 = () in
        let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 1726 "flambda_parser.ml"
        ) = 
# 181 "flambda_parser.mly"
                                          ( Apply_cont (c, None, s) )
# 1730 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv468)) : 'freshtv470)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv471 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUSGREATER ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv472)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typed_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typed_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv459 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv455 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv456)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv457 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)) : 'freshtv460)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv465 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | BLOCK ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | CCALL ->
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv462)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv463 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)) : 'freshtv466)
    | _ ->
        _menhir_fail ()

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_typed_variable_ = 
# 211 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( [] )
# 1838 "flambda_parser.ml"
     in
    _menhir_goto_list_typed_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_SEMICOLON_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv453) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_option_SEMICOLON_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_option_SEMICOLON_) : 'tv_option_SEMICOLON_) = _v in
    ((let _v : 'tv_switch = 
# 163 "flambda_parser.mly"
                      ( [] )
# 1855 "flambda_parser.ml"
     in
    _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)) : 'freshtv454)

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_of_kind_value_ = 
# 211 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( [] )
# 1864 "flambda_parser.ml"
     in
    _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run162 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 56 "flambda_parser.mly"
       (string * char option)
# 1871 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 56 "flambda_parser.mly"
       (string * char option)
# 1882 "flambda_parser.ml"
    )) : (
# 56 "flambda_parser.mly"
       (string * char option)
# 1886 "flambda_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 1892 "flambda_parser.ml"
    ) = let _endpos = _endpos_i_ in
    let _startpos = _startpos_i_ in
    
# 272 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 1898 "flambda_parser.ml"
     in
    _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv450)

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_simple_ = 
# 211 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( [] )
# 1907 "flambda_parser.ml"
     in
    _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 61 "flambda_parser.mly"
       (string)
# 1914 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv447) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 61 "flambda_parser.mly"
       (string)
# 1925 "flambda_parser.ml"
    )) : (
# 61 "flambda_parser.mly"
       (string)
# 1929 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_variable = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 321 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 1937 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv445) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (param : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_typed_variable = 
# 276 "flambda_parser.mly"
                     ( { param; ty = () } )
# 1954 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_typed_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv407 * _menhir_state * 'tv_typed_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState12 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv408)) : 'freshtv410)) : 'freshtv412)) : 'freshtv414)
    | MenhirState118 | MenhirState111 | MenhirState109 | MenhirState103 | MenhirState86 | MenhirState85 | MenhirState79 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv415 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        (_menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) : 'freshtv416)
    | MenhirState147 | MenhirState144 | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv429 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_kinded_variable = 
# 280 "flambda_parser.mly"
                 ( v, None )
# 1987 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinded_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState76 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv417 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
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
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv418)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv419 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
        | MenhirState147 | MenhirState144 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv423 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv424)
        | _ ->
            _menhir_fail ()) : 'freshtv426)) : 'freshtv428)) : 'freshtv430)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv433 * _menhir_state) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv431 * _menhir_state) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_variable)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_named = 
# 154 "flambda_parser.mly"
                      ( Read_mutable v )
# 2052 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)) : 'freshtv434)
    | MenhirState172 | MenhirState149 | MenhirState29 | MenhirState137 | MenhirState73 | MenhirState129 | MenhirState127 | MenhirState121 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv439 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv436)
        | IN | MINUS | MINUSDOT | PLUS | PLUSDOT | SEMICOLON ->
            _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv437 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv438)) : 'freshtv440)
    | MenhirState165 | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv441 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2097 "flambda_parser.ml"
        ) = 
# 271 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 2101 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | _ ->
        _menhir_fail ()) : 'freshtv446)) : 'freshtv448)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 56 "flambda_parser.mly"
       (string * char option)
# 2110 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv405) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 56 "flambda_parser.mly"
       (string * char option)
# 2121 "flambda_parser.ml"
    )) : (
# 56 "flambda_parser.mly"
       (string * char option)
# 2125 "flambda_parser.ml"
    )) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _v : 'tv_const = 
# 293 "flambda_parser.mly"
            ( make_const_int c )
# 2131 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "flambda_parser.mly"
       (string * char option)
# 2138 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv403) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 52 "flambda_parser.mly"
       (string * char option)
# 2148 "flambda_parser.ml"
    )) : (
# 52 "flambda_parser.mly"
       (string * char option)
# 2152 "flambda_parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 294 "flambda_parser.mly"
              ( make_const_float c )
# 2157 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_return_arity = 
# 175 "flambda_parser.mly"
    ( None )
# 2166 "flambda_parser.ml"
     in
    _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL | MINUSGREATER ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | COMMA ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_reduce17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_exn_and_stub = 
# 211 "flambda_parser.mly"
    ( false, false )
# 2190 "flambda_parser.ml"
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
        let (_menhir_stack : 'freshtv397 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv395 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 214 "flambda_parser.mly"
             ( true, true )
# 2212 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)) : 'freshtv398)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv399 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 212 "flambda_parser.mly"
         ( false, true )
# 2223 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv401 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STUB ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv389 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv387 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 215 "flambda_parser.mly"
             ( true, true )
# 2252 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)) : 'freshtv390)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 213 "flambda_parser.mly"
        ( true, false )
# 2263 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv393 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)

and _menhir_goto_of_kind_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2277 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv385 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2285 "flambda_parser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165) : 'freshtv386)

and _menhir_goto_simple : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv341 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv337 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | RBRACE ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv338)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv339 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv347 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv343 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | BLOCK ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | CCALL ->
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv344)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv345 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)) : 'freshtv348)
    | MenhirState103 | MenhirState86 | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
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
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv350)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv353 * _menhir_state * 'tv_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv351 * _menhir_state * 'tv_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (v : 'tv_variable)), _, (s : 'tv_simple)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_named = 
# 155 "flambda_parser.mly"
                                       ( Assign { being_assigned = v; new_value = s } )
# 2417 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)) : 'freshtv354)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv357 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (u : 'tv_unop)), _, (a : 'tv_simple)) = _menhir_stack in
        let _v : 'tv_named = 
# 151 "flambda_parser.mly"
                        ( Prim (Unop (u, a)) )
# 2429 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)) : 'freshtv358)
    | MenhirState172 | MenhirState149 | MenhirState29 | MenhirState137 | MenhirState73 | MenhirState129 | MenhirState127 | MenhirState121 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv361) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_binop = 
# 145 "flambda_parser.mly"
          ( Minus )
# 2448 "flambda_parser.ml"
             in
            _menhir_goto_binop _menhir_env _menhir_stack _v) : 'freshtv360)) : 'freshtv362)
        | MINUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv365) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_binop = 
# 146 "flambda_parser.mly"
             ( Minusdot )
# 2461 "flambda_parser.ml"
             in
            _menhir_goto_binop _menhir_env _menhir_stack _v) : 'freshtv364)) : 'freshtv366)
        | PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv369) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv367) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_binop = 
# 143 "flambda_parser.mly"
         ( Plus )
# 2474 "flambda_parser.ml"
             in
            _menhir_goto_binop _menhir_env _menhir_stack _v) : 'freshtv368)) : 'freshtv370)
        | PLUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv373) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv371) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_binop = 
# 144 "flambda_parser.mly"
            ( Plusdot )
# 2487 "flambda_parser.ml"
             in
            _menhir_goto_binop _menhir_env _menhir_stack _v) : 'freshtv372)) : 'freshtv374)
        | IN | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv375 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (s : 'tv_simple)) = _menhir_stack in
            let _v : 'tv_named = 
# 150 "flambda_parser.mly"
               ( Simple s )
# 2497 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv377 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)) : 'freshtv380)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * _menhir_state * 'tv_simple) * 'tv_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv381 * _menhir_state * 'tv_simple) * 'tv_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (a1 : 'tv_simple)), (b : 'tv_binop)), _, (a2 : 'tv_simple)) = _menhir_stack in
        let _v : 'tv_named = 
# 152 "flambda_parser.mly"
                                      ( Prim (Binop (b, a1, a2)) )
# 2516 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv382)) : 'freshtv384)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program_body_elt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_program_body_elt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv335 * _menhir_state * 'tv_program_body_elt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | EFFECT ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | ROOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | EOF ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176) : 'freshtv336)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_args = 
# 258 "flambda_parser.mly"
    ( [] )
# 2555 "flambda_parser.ml"
     in
    _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run144 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2577 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv283 * _menhir_state * 'tv_named)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2587 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv281 * _menhir_state * 'tv_named)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2593 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (defining_expr : 'tv_named)), _, (body : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2598 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2604 "flambda_parser.ml"
        ) = 
# 188 "flambda_parser.mly"
      ( Let { var = None; kind = None; defining_expr; body } )
# 2608 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)) : 'freshtv284)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv287 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2616 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv285 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2622 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (v : 'tv_kinded_variable)), _, (initial_value : 'tv_simple)), _, (body : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2627 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2636 "flambda_parser.ml"
        ) = 
# 190 "flambda_parser.mly"
      ( let (var, kind) = v in
        Let_mutable { var; kind; initial_value; body } )
# 2641 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv291 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2649 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv289 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2655 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), (v : 'tv_kinded_variable_opt)), _, (defining_expr : 'tv_named)), _, (body : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2660 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2668 "flambda_parser.ml"
        ) = 
# 185 "flambda_parser.mly"
      ( let (var, kind) = v in
        Let { var; kind; defining_expr; body } )
# 2673 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)) : 'freshtv292)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv305 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2681 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv301 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2691 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv299 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2698 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (exn_and_stub : 'tv_exn_and_stub)), _, (name : 'tv_continuation)), _, (params : 'tv_typed_args)), _, (handler : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2703 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _v : 'tv_continuation_handler = 
# 221 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
# 2711 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv297) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_continuation_handler) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState65 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv293 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | BANG | BLOCK | CCALL | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv294)
            | MenhirState134 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | BANG | BLOCK | CCALL | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135) : 'freshtv296)
            | _ ->
                _menhir_fail ()) : 'freshtv298)) : 'freshtv300)) : 'freshtv302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv303 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2756 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)) : 'freshtv306)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv309 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2765 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv307 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2771 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, (recursive : 'tv_recursive)), _, (handler : 'tv_continuation_handler)), _, (t : 'tv_andk)), _, (body : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2776 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2782 "flambda_parser.ml"
        ) = 
# 193 "flambda_parser.mly"
     ( let handlers = handler :: t in
       Let_cont { recursive; body; handlers } )
# 2787 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv319 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2795 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv317 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2801 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s, (name : 'tv_func_sym)), _, (params : 'tv_typed_args)), _, (ret_cont : 'tv_continuation)), (exn_cont : 'tv_option_exn_continuation_)), _, (ret_arity : 'tv_return_arity)), _, (expr : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2806 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _3 = () in
        let _v : 'tv_let_code = 
# 113 "flambda_parser.mly"
  ( ({ name; params; ret_cont; ret_arity; exn_cont; expr } : let_code) )
# 2813 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_code) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv313 * _menhir_state)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_code) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * _menhir_state)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((let_code : 'tv_let_code) : 'tv_let_code) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 101 "flambda_parser.mly"
                                          ( Let_code let_code )
# 2833 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)) : 'freshtv314)) : 'freshtv316)) : 'freshtv318)) : 'freshtv320)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv329 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2841 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv327 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2847 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (c : 'tv_continuation)), _, (v : 'tv_args)), _, (expr : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2852 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_effect = 
# 245 "flambda_parser.mly"
      ( let computation =
          { expr; return_cont = c;
            exception_cont = ("exn", Location.none); computed_values = v }
        in
        { computation = Some computation; static_structure = [] } )
# 2861 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_effect) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_effect) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((e : 'tv_effect) : 'tv_effect) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 99 "flambda_parser.mly"
                                          ( Define_symbol (Nonrecursive, e) )
# 2880 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv322)) : 'freshtv324)) : 'freshtv326)) : 'freshtv328)) : 'freshtv330)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv333 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2888 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv331 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) * _menhir_state * (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2894 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, (c : 'tv_continuation)), _, (v : 'tv_args)), _, (static : 'tv_nonempty_list_static_structure_)), _, (expr : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 2899 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _1 = () in
        let _v : (
# 87 "flambda_parser.mly"
      (Fexpr.definition)
# 2907 "flambda_parser.ml"
        ) = 
# 236 "flambda_parser.mly"
      ( let computation =
          { expr; return_cont = c;
            exception_cont = ("exn", Location.none); computed_values = v }
        in
        { computation = Some computation; static_structure = static } )
# 2915 "flambda_parser.ml"
         in
        _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)) : 'freshtv334)
    | _ ->
        _menhir_fail ()

and _menhir_reduce76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_simple_args = 
# 288 "flambda_parser.mly"
    ( [] )
# 2926 "flambda_parser.ml"
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
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_reduce93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_typed_args = 
# 254 "flambda_parser.mly"
    ( [] )
# 2956 "flambda_parser.ml"
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
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState9
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
        let (_menhir_stack : (('freshtv271 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (h : 'tv_switch_case)), _, (t : 'tv_switch)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch = 
# 165 "flambda_parser.mly"
                                         ( h :: t )
# 2989 "flambda_parser.ml"
         in
        _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv279 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv275 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv273 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (sort : 'tv_switch_sort)), _, (scrutinee : 'tv_simple)), _, (cases : 'tv_switch)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 3011 "flambda_parser.ml"
            ) = 
# 183 "flambda_parser.mly"
    ( Switch {scrutinee; sort; cases} )
# 3015 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv277 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | _ ->
        _menhir_fail ()

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_SEMICOLON_ = 
# 114 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( None )
# 3033 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv267) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let x = () in
    let _v : 'tv_option_SEMICOLON_ = 
# 116 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( Some x )
# 3047 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 56 "flambda_parser.mly"
       (string * char option)
# 3054 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv265) = Obj.magic _menhir_stack in
    let (_endpos_tag_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((tag : (
# 56 "flambda_parser.mly"
       (string * char option)
# 3065 "flambda_parser.ml"
    )) : (
# 56 "flambda_parser.mly"
       (string * char option)
# 3069 "flambda_parser.ml"
    )) = _v in
    let (_startpos_tag_ : Lexing.position) = _startpos in
    ((let _v : 'tv_tag = let _endpos = _endpos_tag_ in
    let _startpos = _startpos_tag_ in
    
# 266 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
# 3077 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv263) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_tag) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv255 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv251 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv253 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv261 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv257 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161) : 'freshtv258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv259 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)
    | _ ->
        _menhir_fail ()) : 'freshtv264)) : 'freshtv266)

and _menhir_goto_option_exn_continuation_ : _menhir_env -> 'ttv_tail -> 'tv_option_exn_continuation_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv243 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | EQUAL ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv244)

and _menhir_goto_recursive : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_recursive -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv237 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv238)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv241 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LETK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState152 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153) : 'freshtv240)
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv242)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_program_body_elt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_program_body_elt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * _menhir_state * 'tv_program_body_elt) * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv219 * _menhir_state * 'tv_program_body_elt) * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_program_body_elt)), _, (xs : 'tv_list_program_body_elt_)) = _menhir_stack in
        let _v : 'tv_list_program_body_elt_ = 
# 213 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( x :: xs )
# 3256 "flambda_parser.ml"
         in
        _menhir_goto_list_program_body_elt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv231 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv229 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (elts : 'tv_list_program_body_elt_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 86 "flambda_parser.mly"
      (Fexpr.program)
# 3275 "flambda_parser.ml"
            ) = 
# 95 "flambda_parser.mly"
                                       ( elts )
# 3279 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv227) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 86 "flambda_parser.mly"
      (Fexpr.program)
# 3287 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 86 "flambda_parser.mly"
      (Fexpr.program)
# 3295 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 86 "flambda_parser.mly"
      (Fexpr.program)
# 3303 "flambda_parser.ml"
            )) : (
# 86 "flambda_parser.mly"
      (Fexpr.program)
# 3307 "flambda_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv224)) : 'freshtv226)) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 80 "flambda_parser.mly"
       (string)
# 3323 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv217) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 80 "flambda_parser.mly"
       (string)
# 3334 "flambda_parser.ml"
    )) : (
# 80 "flambda_parser.mly"
       (string)
# 3338 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_symbol = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 313 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 3346 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv215) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_symbol) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (s : 'tv_symbol)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 102 "flambda_parser.mly"
                                          ( Root s )
# 3364 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)) : 'freshtv188)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_func_sym = 
# 309 "flambda_parser.mly"
               ( s )
# 3376 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_sym) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_func_sym) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | MINUSGREATER ->
            _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv190)) : 'freshtv192)) : 'freshtv194)) : 'freshtv196)
    | MenhirState172 | MenhirState149 | MenhirState29 | MenhirState137 | MenhirState73 | MenhirState129 | MenhirState127 | MenhirState121 | MenhirState118 | MenhirState81 | MenhirState111 | MenhirState109 | MenhirState103 | MenhirState86 | MenhirState85 | MenhirState79 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_simple = 
# 303 "flambda_parser.mly"
               ( Symbol s )
# 3405 "flambda_parser.ml"
         in
        _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)) : 'freshtv200)
    | MenhirState152 | MenhirState169 | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BLOCK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv201 * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv202)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv203 * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)) : 'freshtv210)
    | MenhirState165 | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3455 "flambda_parser.ml"
        ) = 
# 270 "flambda_parser.mly"
               ( Symbol s )
# 3459 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)) : 'freshtv214)
    | _ ->
        _menhir_fail ()) : 'freshtv216)) : 'freshtv218)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 61 "flambda_parser.mly"
       (string)
# 3468 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 61 "flambda_parser.mly"
       (string)
# 3479 "flambda_parser.ml"
    )) : (
# 61 "flambda_parser.mly"
       (string)
# 3483 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_continuation = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 330 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 3491 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv181) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_continuation) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv141 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv136)
        | COLON | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
            ((let _v : 'tv_option_exn_continuation_ = 
# 114 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( None )
# 3523 "flambda_parser.ml"
             in
            _menhir_goto_option_exn_continuation_ _menhir_env _menhir_stack _v) : 'freshtv138)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv139 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)) : 'freshtv142)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv149) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, (cont : 'tv_continuation)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_continuation = 
# 106 "flambda_parser.mly"
                             ( cont )
# 3543 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147) = _menhir_stack in
        let (_v : 'tv_exn_continuation) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
        let (_v : 'tv_exn_continuation) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
        let ((x : 'tv_exn_continuation) : 'tv_exn_continuation) = _v in
        ((let _v : 'tv_option_exn_continuation_ = 
# 116 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( Some x )
# 3557 "flambda_parser.ml"
         in
        _menhir_goto_option_exn_continuation_ _menhir_env _menhir_stack _v) : 'freshtv144)) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)) : 'freshtv152)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv165 * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : 'tv_tag)), _, (c : 'tv_continuation)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch_case = 
# 159 "flambda_parser.mly"
                                          ( i,c )
# 3570 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_switch_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | RBRACE ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv154)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (c : 'tv_switch_case)) = _menhir_stack in
            let _v : 'tv_switch = 
# 164 "flambda_parser.mly"
                    ( [c] )
# 3605 "flambda_parser.ml"
             in
            _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)) : 'freshtv162)) : 'freshtv164)) : 'freshtv166)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LBRACE ->
            _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv168)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEF | EFFECT | EOF | LET | RBRACE | ROOT ->
            _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv170)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv171 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv172)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv175 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv173 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s), (func : 'tv_csymbol)), _, (args : 'tv_simple_args)), _, (ra : 'tv_return_arity)), _, (r : 'tv_continuation)), _, (e : 'tv_continuation)) = _menhir_stack in
        let _7 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.expr)
# 3668 "flambda_parser.ml"
        ) = 
# 197 "flambda_parser.mly"
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
# 3682 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)) : 'freshtv176)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | BANG | BLOCK | CCALL | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143) : 'freshtv178)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | LBRACE ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv180)
    | _ ->
        _menhir_fail ()) : 'freshtv182)) : 'freshtv184)

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_recursive = 
# 117 "flambda_parser.mly"
    ( Nonrecursive )
# 3721 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_recursive = 
# 118 "flambda_parser.mly"
        ( Recursive )
# 3735 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v) : 'freshtv134)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * 'tv_program_body_elt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv9 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 88 "flambda_parser.mly"
      (Fexpr.static_structure)
# 3757 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3766 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv15 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv17 * _menhir_state * 'tv_symbol))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv39 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv47 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state) * 'tv_kinded_variable_opt)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_named)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * 'tv_simple) * 'tv_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_variable)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv61 * _menhir_state) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv65 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv67 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv69 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv71 * _menhir_state)) * 'tv_csymbol)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv81 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv83 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv87 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * 'tv_switch_case)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv101 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * 'tv_tag_to_size)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv108)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv109 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv115 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv118)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv119 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_typed_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_func_sym) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv132)

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_program_body_elt_ = 
# 211 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
    ( [] )
# 4068 "flambda_parser.ml"
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

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_run151 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | LETK | UIDENT _ ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151

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
# 86 "flambda_parser.mly"
      (Fexpr.program)
# 4154 "flambda_parser.ml"
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
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EFFECT ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ROOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 332 "flambda_parser.mly"
  

# 4191 "flambda_parser.ml"

# 269 "/home/chambart/.opam/4.10.0+build-flambda2/lib/menhir/standard.mly"
  

# 4196 "flambda_parser.ml"
