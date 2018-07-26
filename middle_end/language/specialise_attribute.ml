
type specialise_attribute =
  | Always_specialise
  | Never_specialise
  | Default_specialise

let print_specialise_attribute ppf attr =
  let fprintf = Format.fprintf in
  match attr with
  | Always_specialise -> fprintf ppf "Always_specialise"
  | Never_specialise -> fprintf ppf "Never_specialise"
  | Default_specialise -> fprintf ppf "Default_specialise"
