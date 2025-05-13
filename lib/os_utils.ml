open Core

module OSUtils = struct
  let code_tree_path = ".docktrack/code_tree.sexp"

  let validate_file_path file_path () =
    match Sys_unix.file_exists file_path with `Yes -> true | _ -> false

  let extract_file_name path_str = Filename.basename path_str

  let validate_dir dir () =
    match Sys_unix.is_directory dir with `Yes -> true | _ -> false

  let create_dir dir () = if not (validate_dir dir ()) then Core_unix.mkdir dir

  let concat_path path len =
    let path_parts = Filename.parts path in
    let path_length = List.length path_parts in
    let parsed_parts =
      if path_length > 5 then
        "...." :: List.sub path_parts ~pos:(path_length - len) ~len:len
      else path_parts
    in
    Filename.of_parts parsed_parts
end
