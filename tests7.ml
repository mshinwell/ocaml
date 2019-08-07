external ( + ) : int -> int -> int = "%addint"

module String = struct
  type t = string
  external length : t -> int = "%string_length"
end

let cow = "Cow"
let sheep = "Sheep"

let f x str =
  x + String.length cow + String.length sheep
    + String.length str + String.length str
