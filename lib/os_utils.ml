open Core

module OSUtils = struct
  let validate_file_path file_path () =
    match Sys_unix.file_exists file_path with `Yes -> true | _ -> false

  let extract_file_name path_str = Filename.basename path_str
end
