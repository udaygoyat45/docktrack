open Core

type cmd_parsed_type = GitCommand | DocktrackCommand

let parse_docktrack_cmd cmd args (code_tree : Code_tree.CodeTree.ct) =
  let ft = code_tree.feature_tree in
  match cmd :: args with
  | [ "view-code-tree" ] ->
      Code_tree.CodeTree.print_code_tree code_tree;
      code_tree
  | [ "view-features" ] ->
      Feature_tree.FeatureTree.print_tree ft;
      code_tree
  | [ "view-feature"; ft_name ] ->
      Feature_tree.FeatureTree.print_feature ft_name ft;
      code_tree
  | [ "add-file" ] ->
      let n_file_path =
        Cli_utils.CliUtils.inline_input "File Path"
          (fun path -> Os_utils.OSUtils.validate_file_path path ())
          "Please provide an accurate path from the root of the project dir"
      in
      let rec add_features features () =
        match Cli_utils.CliUtils.inline_input_bool "Add feature to file?" with
        | Yes ->
            let n_ft_name =
              Cli_utils.CliUtils.inline_input "Feature name"
                (Code_tree.CodeTree.feature_exists code_tree)
                "No such feature exists within the feature tree. Use \
                 'dock_add_feature' to add a feature"
            in
            let features' = Set.add features n_ft_name in
            add_features features' ()
        | No -> features
      in
      let empty_set = Set.empty (module String) in
      let c_features = add_features empty_set () in
      let file_name = Os_utils.OSUtils.extract_file_name n_file_path in
      let file_data : Code_tree.CodeTree.file =
        { path = n_file_path; name = file_name; feature_names = c_features }
      in

      let code_tree' =
        try Code_tree.CodeTree.add_file file_data code_tree with
        | Code_tree.CodeTree.DuplicateFile name ->
            Printf.printf "File %s already exists in the code tree" name;
            code_tree
        | Code_tree.CodeTree.MissingFile path ->
            Printf.printf "Invalid file path %s. Please provide a valid path"
              path;
            code_tree
      in
      code_tree'
  | [ "remove-file"; file_path ] ->
      let code_tree' =
        try Code_tree.CodeTree.remove_file file_path code_tree with
        | Code_tree.CodeTree.InvalidFilePath file_path ->
            Printf.printf "No file found at this path: %s\n" file_path;
            code_tree
        | Code_tree.CodeTree.MissingFile file_path ->
            Printf.printf "No file found in the code tree with this path: %s\n"
              file_path;
            code_tree
      in
      code_tree'
  | [ "add-feature" ] ->
      let n_ft =
        Cli_utils.CliUtils.inline_input "Feature"
          (fun x -> String.length x > 0)
          "Feature name cannot be empty"
      in
      let n_pft =
        Cli_utils.CliUtils.inline_input ~optional:ft.root_name
          "Parent (optional)"
          (fun _ -> true)
          "Parent feature name cannot be empty"
      in
      let n_title =
        Cli_utils.CliUtils.inline_input "Feature title"
          (fun x -> String.length x > 0)
          "Feature title cannot be empty"
      in
      let n_descr =
        Cli_utils.CliUtils.inline_input ~optional:""
          "Feature description (optional)"
          (fun x -> String.length x > 10)
          "Feature description should be more than 10 characters"
      in
      let n_url =
        Cli_utils.CliUtils.inline_input ~optional:"" "Feature URL (optional)"
          (fun x -> String.length x = 0 || Ds_utils.is_valid_http_url x)
          "Please provide a valid http(s) URL"
      in
      Cli_utils.CliUtils.print_header "Feature Metadata";
      printf "feature: %s, parent: %s, metadata: %s; %s; %s\n" n_ft n_pft
        n_title n_descr n_url;
      let n_metadata : Feature_tree.FeatureTree.m =
        {
          title = n_title;
          descr = (if String.length n_descr = 0 then Some n_descr else None);
          url = (if String.length n_url = 0 then Some n_url else None);
        }
      in
      let ft' =
        try Feature_tree.FeatureTree.add_feature n_ft n_pft n_metadata ft with
        | Feature_tree.FeatureTree.MissingParentFeature name ->
            Printf.printf "No such feature (parent) %s in the feature tree" name;
            ft
        | Feature_tree.FeatureTree.DuplicateFeatureName name ->
            Printf.printf "Feature %s already exists in the feature tree" name;
            ft
      in
      { code_tree with feature_tree = ft' }
  | [ "remove-feature"; ft_name ] ->
      let code_tree' =
        try Code_tree.CodeTree.remove_feature ft_name code_tree with
        | Feature_tree.FeatureTree.DeletingProjectRoot msg ->
            Printf.printf "Cannot delete the project root feature %s" msg;
            code_tree
        | Feature_tree.FeatureTree.MissingFeature _ ->
            Printf.printf "No such feature with name %s exists\n" ft_name;
            code_tree
      in
      code_tree'
  | [ "add-update" ] ->
      let feature_name =
        Cli_utils.CliUtils.inline_input "Feature"
          (fun feature -> Feature_tree.FeatureTree.feature_exists feature ft)
          "Please provide a valid feature name. Use 'dock_view_features' to \
           view the feature tree"
      in
      let update_title =
        Cli_utils.CliUtils.inline_input "Update title"
          (fun title -> String.length title > 0)
          "Update title cannot be empty"
      in
      let update_content =
        Cli_utils.CliUtils.inline_input "Update content"
          (fun content -> String.length content > 0)
          "Update content cannot be empty"
      in
      let timestamp = Core_unix.gettimeofday () in
      let update =
        {
          Feature_update.FeatureUpdate.title = update_title;
          content = update_content;
          timestamp = int_of_float timestamp;
          status = Feature_update.FeatureUpdate.Undocumented;
        }
      in
      let ft' =
        try Feature_tree.FeatureTree.add_update update feature_name ft
        with Feature_tree.FeatureTree.MissingFeature name ->
          Printf.printf "No such feature %s in the feature tree" name;
          ft
      in
      { code_tree with feature_tree = ft' }
  | [ "view-updates"; ft_name ] ->
      if Feature_tree.FeatureTree.feature_exists ft_name ft then
        Feature_tree.FeatureTree.print_updates ft_name ft
      else Printf.printf "No such feature %s in the feature tree" ft_name;
      code_tree
  | [ "view-update"; ft_name; update_name ] ->
      let update_str =
        try
          Feature_tree.FeatureTree.string_of_update ft_name update_name ft
        with
        | Feature_tree.FeatureTree.MissingUpdate (name, update_name) ->
            Printf.sprintf "No such update %s in the feature %s\n" update_name
              name
        | Feature_tree.FeatureTree.MissingFeature name ->
            Printf.sprintf "No such feature %s in the feature tree" name
      in
      Printf.printf "%s\n" update_str;
      code_tree
  | [ "document-next-update"; ft_name ] ->
      let ft' =
        try
          let ft'' = Feature_tree.FeatureTree.document_next_update ft_name ft in
          let upd =
            match
              Feature_tree.FeatureTree.newest_documented_update ft_name ft''
            with
            | Some u -> u
            | None -> failwith "No documented updates available"
          in
          Printf.printf "Documented feature %s\n"
            (Feature_update.FeatureUpdate.string_of_update upd);
          ft''
        with
        | Feature_tree.FeatureTree.MissingFeature name ->
            Printf.printf "No such feature %s in the feature tree\n" name;
            ft
        | Feature_tree.FeatureTree.EmptyUndocumentedUpdates ->
            Printf.printf "No undocumented updates available for feature %s\n"
              ft_name;
            ft
      in
      { code_tree with feature_tree = ft' }
  | [ "document-update"; ft_name; update_name ] ->
      let ft' =
        try
          let ft'' =
            Feature_tree.FeatureTree.document_update ft_name update_name ft
          in
          Printf.printf "Documented update: %s\n"
            (Feature_tree.FeatureTree.string_of_update ft_name update_name ft);
          ft''
        with
        | Feature_tree.FeatureTree.MissingFeature name ->
            Printf.printf "No such feature %s in the feature tree\n" name;
            ft
        | Feature_tree.FeatureTree.MissingUpdate (name, update_name) ->
            Printf.printf "No such update %s in the feature %s\n" update_name
              name;
            ft
      in
      { code_tree with feature_tree = ft' }
  | [ "remove-update"; ft_name; update_name ] ->
      let ft' =
        try
          let ft'' =
            Feature_tree.FeatureTree.remove_update ft_name update_name ft
          in
          Printf.printf "Removed update %s\n"
            (Feature_tree.FeatureTree.string_of_update ft_name update_name ft);
          ft''
        with
        | Feature_tree.FeatureTree.MissingFeature name ->
            Printf.printf "Docktrack: No such feature %s in the feature tree\n"
              name;
            ft
        | Feature_tree.FeatureTree.MissingUpdate (name, update_name) ->
            Printf.printf "Docktrack: No such update %s in the feature %s\n"
              update_name name;
            ft
      in
      { code_tree with feature_tree = ft' }
  | [ "add-feature-to-files"; ft_name ] ->
      let feature_exists = Feature_tree.FeatureTree.feature_exists ft_name ft in
      let rec query_file_names () =
        let add_query =
          Cli_utils.CliUtils.inline_input_bool "Add a new file name"
        in
        match add_query with
        | Yes ->
            let file_name =
              Cli_utils.CliUtils.inline_input "File path"
                (fun x -> Map.mem code_tree.file_map x)
                "Enter valid file paths present within the code tree"
            in
            file_name :: query_file_names ()
        | No -> []
      in
      let attached_files =
        match feature_exists with
        | true -> query_file_names ()
        | false ->
            Printf.printf "%s\n" "No such feature name exists";
            []
      in
      let file_map' =
        Map.filter_map code_tree.file_map ~f:(fun file ->
            if List.mem attached_files file.path ~equal:String.equal then
              Some
                { file with feature_names = Set.add file.feature_names ft_name }
            else None)
      in
      { code_tree with file_map = file_map' }
  | _ ->
      Printf.printf "%s\n"
        "Docktrack: Invalid command. Please use a valid docktrack command.";
      code_tree

let simulate_command_shell cmd subcmd args code_tree =
  match cmd with
  | GitCommand ->
      let shell_git_cmd = String.concat ~sep:" " ("git" :: subcmd :: args) in
      let git_output = Cli_utils.CliUtils.run_unix shell_git_cmd in
      Cli_utils.CliUtils.print_header "Git";
      Printf.printf "%s\n" git_output;
      Cli_utils.CliUtils.print_header "Post-Git Docktrack Validator";
      Code_tree.CodeTree.validate_code_tree code_tree ()
  | DocktrackCommand ->
      Cli_utils.CliUtils.print_header "Docktrack";
      parse_docktrack_cmd subcmd args code_tree

let _ =
  let read_code_tree =
    if Os_utils.OSUtils.validate_file_path Os_utils.OSUtils.code_tree_path ()
    then Code_tree.CodeTree.read_code_tree ()
    else
      Code_tree.CodeTree.empty_ct "docktrack"
        { title = "root"; descr = None; url = None }
  in
  let ref_code_tree = ref read_code_tree in
  let handler cmd subcmd args =
    let old_tree = !ref_code_tree in
    let new_tree = simulate_command_shell cmd subcmd args old_tree in
    ref_code_tree := new_tree;
    Code_tree.CodeTree.save_code_tree new_tree ()
  in
  let git_handler = handler GitCommand in
  let docktrack_handler = handler DocktrackCommand in
  (* Input from the CLI args - similar to what the final product would do *)

  Command_unix.run
    (Cli_utils.CliUtils.git_docktrack_combined "Docktrack is goated" git_handler
       docktrack_handler)
