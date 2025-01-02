module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

module SetUtils = struct

let print_string_set set = 
  StringSet.iter (fun x -> Printf.printf "%s " x) set; 
  Printf.printf "\n"
end

module MapUtils = struct
  let print_string_map map = 
    StringMap.iter (fun key _ -> Printf.printf "%s " key) map;
    Printf.printf "\n"
end