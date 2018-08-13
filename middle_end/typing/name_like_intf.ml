module type S = sig
  type t

  include Map.With_set with type t := t
  include Contains_names.S with type t := t
end
