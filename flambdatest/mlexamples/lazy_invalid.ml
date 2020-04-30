let x = (fun () -> (fun _ -> Lazy.force (lazy "")) ()) ()

