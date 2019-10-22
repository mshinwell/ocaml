external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"

let f b = if b then 1 else 0
