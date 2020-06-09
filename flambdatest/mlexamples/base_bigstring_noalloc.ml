external int64_to_int : int64 -> int = "%int64_to_int"
external swap64 : int64 -> int64 = "%bswap_int64"
external unsafe_get_64 : 'a -> int -> int64 = "%caml_bigstring_get64u"

let arch_big_endian = Sys.big_endian
let arch_sixtyfour = Sys.word_size = 64

let unsafe_read_int64 t ~pos = unsafe_get_64 t pos
let unsafe_read_int64_swap t ~pos = swap64 (unsafe_get_64 t pos)

let unsafe_get_int64_t_be =
  if arch_big_endian then unsafe_read_int64 else unsafe_read_int64_swap

let int64_conv_error () =
  failwith "unsafe_read_int64: value cannot be represented unboxed!"

let int64_to_int_exn n =
  if arch_sixtyfour
  then
    if n >= -0x4000_0000_0000_0000L && n < 0x4000_0000_0000_0000L
    then int64_to_int n
    else int64_conv_error ()
  else if n >= -0x0000_0000_4000_0000L && n < 0x0000_0000_4000_0000L
  then int64_to_int n
  else int64_conv_error ()

let unsafe_get_int64_be_exn t ~pos = int64_to_int_exn (unsafe_get_int64_t_be t ~pos)

