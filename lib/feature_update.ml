module type FeatureUpdate = sig
  type t
  val compare : t -> t -> int
end

module FeatureUpdate = struct
  type status = Documented | Undocumented

  type t = {
    title: string;
    content: string;
    timestamp: int;
    status: status;
  }

  let compare a b = String.compare a.title b.title

  let string_of_update update = 
    Printf.sprintf "%s\n" update.title

  let string_of_update_verbose update = 
    Printf.sprintf "Title: %s\n%s\n" update.title update.content

  let string_of_updates updates = 
    List.fold_left (fun acc update -> acc ^ "- " ^ update.title ^ "\n") "" updates
end

module FeatureUpdateSet = Set.Make(FeatureUpdate)