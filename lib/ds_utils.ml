open Core

module SetUtils = struct
  let string_of_string_set ?(sep = ' ') set =
    let lst = Set.to_list set in
    match lst with
    | [] -> ""
    | hd :: rest ->
        hd
        ^ List.fold_left rest ~init:"" ~f:(fun acc x ->
              acc ^ String.make 1 sep ^ x)

  let print_string_set ?(sep = ' ') set =
    Printf.printf "%s\n" (string_of_string_set ~sep set)
end

let bind_exn opt exn = match opt with Some x -> x | None -> raise exn

let is_valid_http_url (s : string) : bool =
  match try Some (Uri.of_string s) with _ -> None with
  | None -> false
  | Some uri ->
      (match Uri.scheme uri with Some ("http" | "https") -> true | _ -> false)
      && Option.is_some (Uri.host uri)
