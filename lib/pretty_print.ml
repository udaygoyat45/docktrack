(* open Feature_tree *)

module PrettyPrint = struct
  let indent = 2
  let bar = "│" 
  let branch = "├─"
  let block = bar ^ (String.make indent ' ')
  let n_block n = String.concat "" (List.init n (fun _ -> block))
end