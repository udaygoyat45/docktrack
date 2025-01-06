module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

module SetUtils = struct

let string_of_string_set ?(sep=' ') set  = 
  let lst = StringSet.to_list set in
  match lst with
  | [] -> ""
  | hd ::rest -> hd ^ List.fold_left (fun acc x -> acc ^ (String.make 1 sep) ^ x) "" rest

let print_string_set ?(sep=' ') set = 
  Printf.printf "%s\n" (string_of_string_set ~sep:(sep) set)
end

module MapUtils = struct
  let print_string_map ?(sep=' ') map =
    let lst = StringMap.bindings map |> List.map fst in
    List.iteri (fun i x ->
      if (i < List.length lst - 1) then
        Printf.printf "%s%c" x sep
      else
        Printf.printf "%s\n" x
    ) lst
end