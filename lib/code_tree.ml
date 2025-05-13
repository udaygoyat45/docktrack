open Core

module CodeTree = struct
  type sst = Set.M(String).t [@@deriving sexp]

  type file = { name : string; path : string; feature_names : sst }
  [@@deriving sexp]

  type smp = file Map.M(String).t [@@deriving sexp]

  type ct = { file_map : smp; feature_tree : Feature_tree.FeatureTree.ft }
  [@@deriving sexp]

  exception DuplicateFile of string
  exception MissingFile of string
  exception InvalidFilePath of string

  let empty_ct project_name project_data =
    {
      file_map = Map.empty (module String);
      feature_tree =
        Feature_tree.FeatureTree.empty_feature_tree project_name project_data;
    }

  let add_file file_data { file_map; feature_tree } =
    let file =
      {
        name = file_data.name;
        path = file_data.path;
        feature_names = Set.empty (module String);
      }
    in
    let file_map' = Map.add file_map ~key:file_data.path ~data:file in
    match file_map' with
    | `Ok map -> { file_map = map; feature_tree }
    | `Duplicate -> raise (DuplicateFile file_data.name)

  let update_file new_file_data { file_map; _ } =
    let a = Map.remove file_map new_file_data.path in
    Map.add a ~key:new_file_data.path ~data:new_file_data

  let remove_file file_path { file_map; feature_tree } =
    if not (Os_utils.OSUtils.validate_file_path file_path ()) then
      raise (InvalidFilePath file_path)
    else if not (Map.mem file_map file_path) then raise (MissingFile file_path)
    else
      let file_map' = Map.remove file_map file_path in
      { feature_tree; file_map = file_map' }
  

  let remove_feature feature_name { file_map; feature_tree } =
    let feature_tree' =
      Feature_tree.FeatureTree.remove_feature feature_name feature_tree
    in
    let file_map' =
      Map.map file_map ~f:(fun file ->
          {
            file with
            feature_names = Set.remove file.feature_names feature_name;
          })
    in
    { file_map = file_map'; feature_tree = feature_tree' }

  let feature_exists { feature_tree; _ } feature_name =
    Map.mem feature_tree.feature_map feature_name

  let print_code_tree { file_map; _ } =
    if Map.is_empty file_map then
      Printf.printf "There are no files associated with any features yet!\n"
    else
      Map.iter file_map ~f:(fun file ->
          let feature_names =
            Ds_utils.SetUtils.string_of_string_set ~sep:',' file.feature_names
          in
          let trimmed_path = Os_utils.OSUtils.concat_path file.path 5 in
          Printf.printf "File: %s\nFeatures: %s\n" trimmed_path feature_names)

  let save_code_tree code_tree () =
    Os_utils.OSUtils.create_dir ".docktrack" ();
    let write_path = Filename.concat ".docktrack" "code_tree.sexp" in
    let sexp = sexp_of_ct code_tree in
    Out_channel.with_file write_path ~f:(fun oc -> Sexp.output oc sexp)

  let read_code_tree () =
    Os_utils.OSUtils.create_dir ".docktrack" ();
    let read_path = Filename.concat ".docktrack" "code_tree.sexp" in
    let data = In_channel.read_all read_path in
    let sexp = Sexp.of_string data in
    ct_of_sexp sexp

  let validate_code_tree { file_map; feature_tree } () =
    let missing_files =
      Map.fold file_map ~init:[] ~f:(fun ~key:_ ~data:file acc ->
          if not (Os_utils.OSUtils.validate_file_path file.path ()) then
            file :: acc
          else acc)
    in
    let rec update_missing_files missing_files file_map' =
      match missing_files with
      | file :: rest ->
          let error_msg =
            Printf.sprintf
              "FILE: %s IS MISSING\n\
               Would you like to update location\n\
               P.S. Choosing NO would lead to removal from code tree"
              file.path
          in
          let keep_query = Cli_utils.CliUtils.inline_input_bool error_msg in
          let file_map' =
            match keep_query with
            | Cli_utils.CliUtils.Yes ->
                let new_path =
                  Cli_utils.CliUtils.inline_input_multi "New file path"
                    [
                      {
                        validator = (fun x -> String.length x > 0);
                        error_msg = "The file length cannot be empty";
                      };
                      {
                        validator =
                          (fun x -> Os_utils.OSUtils.validate_file_path x ());
                        error_msg = "Please provide a valid, regular file path";
                      };
                      {
                        validator =
                          (fun x ->
                            let c_file_name =
                              Os_utils.OSUtils.extract_file_name x
                            in
                            not (Map.mem file_map' c_file_name));
                        error_msg =
                          "This file path already exists within the Code Tree.";
                      };
                    ]
                    "Please provide a valid regular file path"
                in
                let new_name = Os_utils.OSUtils.extract_file_name new_path in
                let new_file = { file with name = new_name; path = new_path } in
                Map.update file_map' file.path ~f:(fun _ -> new_file)
            | Cli_utils.CliUtils.No -> Map.remove file_map' file.path
          in
          update_missing_files rest file_map'
      | [] -> file_map'
    in
    let updated_file_map = update_missing_files missing_files file_map in
    if List.length missing_files > 0 then
      Printf.printf "The code tree been validated\n"
    else Printf.printf "No files need validation\n";
    { file_map = updated_file_map; feature_tree }
end
