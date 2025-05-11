open Core

module OSUtils = struct
  let code_tree_path = ".docktrack/code_tree.sexp"

  let validate_file_path file_path () =
    match Sys_unix.file_exists file_path with `Yes -> true | _ -> false

  let extract_file_name path_str = Filename.basename path_str

  let validate_dir dir () =
    match Sys_unix.is_directory dir with `Yes -> true | _ -> false

  let create_dir dir () = if not (validate_dir dir ()) then Core_unix.mkdir dir
end
