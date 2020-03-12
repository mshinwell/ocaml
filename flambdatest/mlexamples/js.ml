module A = struct
  let init x y = assert false
  let of__unsafe a = assert false
  let of__debug a = assert false
  let of_ = if false then of__debug else of__unsafe
end
