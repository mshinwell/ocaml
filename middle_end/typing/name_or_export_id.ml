type t =
  | Name of Name.t
  | Export_id of Export_id.t

include Hashtbl.Make_with_map (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Name _, Export_id _ -> -1
    | Export_id _, Name _ -> 1
    | Name name1, Name name2 -> Name.compare name1 name2
    | Export_id id1, Export_id id2 -> Export_id.compare id1 id2

  let hash t =
    match t with
    | Name name -> Hashtbl.hash (0, Name.hash name)
    | Export_id id -> Hashtbl.hash (1, Export_id.hash id)

  let print ppf t =
    match t with
    | Name name -> Name.print ppf name
    | Export_id id -> Export_id.print ppf id
end)
