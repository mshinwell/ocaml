
type inline_attribute =
  | Always_inline
  | Never_inline
  | Unroll of int
  | Default_inline

let print_inline_attribute ppf attr =
  let fprintf = Format.fprintf in
  match attr with
  | Always_inline -> fprintf ppf "Always_inline"
  | Never_inline -> fprintf ppf "Never_inline"
  | Unroll n -> fprintf ppf "@[(Unroll %d)@]" n
  | Default_inline -> fprintf ppf "Default_inline"
