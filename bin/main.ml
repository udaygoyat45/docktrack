open Core

type cmd_parsed_type = GitCommand | DocktrackCommand | InvalidCommand

let valid_git_subcmd parsed_cmd =
  let supported_git_cmds =
    [
      "init";
      "clone";
      "status";
      "add";
      "commit";
      "push";
      "pull";
      "fetch";
      "branch";
      "checkout";
      "merge";
      "rebase";
      "log";
      "diff";
      "reset";
      "revert";
      "tag";
      "stash";
      "remote";
      "show";
    ]
  in
  match parsed_cmd with
  | [] -> false
  | first :: _ -> List.mem supported_git_cmds first ~equal:String.equal

let valid_docktrack_cmd parsed_cmd =
  let docktrack_subcmds =
    [
      "dock_add_feature";
      "dock_remove_feature";
      "dock_add_file";
      "dock_remove_files";
      "dock_view_code_tree";
      "dock_view_features";
      "dock_view_feature";
      "dock_add_update";
      "dock_view_updates";
      "dock_view_update";
      "dock_document_next_update";
      "dock_document_update";
      "dock_remove_update";
    ]
  in
  match parsed_cmd with
  | [] -> false
  | first :: _ -> List.mem docktrack_subcmds first ~equal:String.equal

let parse_docktrack_cmd cmd (code_tree : Code_tree.CodeTree.ct) =
  let ft = code_tree.feature_tree in
  match cmd with
  | [ "dock_view_code_tree" ] ->
      Code_tree.CodeTree.print_code_tree code_tree;
      code_tree
  | [ "dock_view_features" ] ->
      Feature_tree.FeatureTree.print_tree ft;
      code_tree
  | [ "dock_view_feature"; ft_name ] ->
      Feature_tree.FeatureTree.print_feature ft_name ft;
      code_tree
  | [ "dock_add_file" ] ->
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
            Printf.eprintf "Docktrack: File %s already exists in the code tree"
              name;
            code_tree
        | Code_tree.CodeTree.MissingFile path ->
            Printf.eprintf
              "Docktrack: Invalid file path %s. Please provide a valid path"
              path;
            code_tree
      in
      code_tree'
  | [ "dock_remove_files" ] -> code_tree
  | [ "dock_add_feature" ] ->
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
            Printf.eprintf
              "Docktrack: No such feature (parent) %s in the feature tree" name;
            ft
        | Feature_tree.FeatureTree.DuplicateFeatureName name ->
            Printf.eprintf
              "Docktrack: Feature %s already exists in the feature tree" name;
            ft
      in
      { code_tree with feature_tree = ft' }
  | "dock_remove_feature" :: ft_name ->
      let ft_name' = String.concat ~sep:" " ft_name in
      let ft' =
        try Feature_tree.FeatureTree.remove_feature ft_name' ft
        with Feature_tree.FeatureTree.DeletingProjectRoot msg ->
          Printf.eprintf "Docktrack: Cannot delete the project root feature %s"
            msg;
          ft
      in
      { code_tree with feature_tree = ft' }
  | [ "dock_add_update" ] ->
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
          Printf.eprintf "Docktrack: No such feature %s in the feature tree"
            name;
          ft
      in
      { code_tree with feature_tree = ft' }
  | [ "dock_view_updates"; ft_name ] ->
      if Feature_tree.FeatureTree.feature_exists ft_name ft then
        Feature_tree.FeatureTree.print_updates ft_name ft
      else
        Printf.eprintf "Docktrack: No such feature %s in the feature tree"
          ft_name;
      code_tree
  | [ "dock_view_update"; ft_name; update_name ] ->
      let update_str =
        try
          Feature_tree.FeatureTree.string_of_update ft_name update_name ft
        with
        | Feature_tree.FeatureTree.MissingUpdate (name, update_name) ->
            Printf.sprintf "Docktrack: No such update %s in the feature %s\n"
              update_name name
        | Feature_tree.FeatureTree.MissingFeature name ->
            Printf.sprintf "Docktrack: No such feature %s in the feature tree"
              name
      in
      Printf.eprintf "%s\n" update_str;
      code_tree
  | [ "dock_document_next_update"; ft_name ] ->
      let ft' =
        try
          let ft'' = Feature_tree.FeatureTree.document_next_update ft_name ft in
          (* now pass ft'' into newest_documented_update *)
          let upd =
            match
              Feature_tree.FeatureTree.newest_documented_update ft_name ft''
            with
            | Some u -> u
            | None -> failwith "No documented updates available"
          in
          Printf.eprintf "Docktrack: Documented feature %s\n"
            (Feature_update.FeatureUpdate.string_of_update upd);
          ft''
        with
        | Feature_tree.FeatureTree.MissingFeature name ->
            Printf.eprintf "Docktrack: No such feature %s in the feature tree\n"
              name;
            ft
        | Feature_tree.FeatureTree.EmptyUndocumentedUpdates ->
            Printf.eprintf
              "Docktrack: No undocumented updates available for feature %s\n"
              ft_name;
            ft
      in
      { code_tree with feature_tree = ft' }
  | [ "dock_document_update"; ft_name; update_name ] ->
      let ft' =
        try
          let ft'' =
            Feature_tree.FeatureTree.document_update ft_name update_name ft
          in
          Printf.eprintf "Docktrack: Documented update: %s\n"
            (Feature_tree.FeatureTree.string_of_update ft_name update_name ft);
          ft''
        with
        | Feature_tree.FeatureTree.MissingFeature name ->
            Printf.eprintf "Docktrack: No such feature %s in the feature tree\n"
              name;
            ft
        | Feature_tree.FeatureTree.MissingUpdate (name, update_name) ->
            Printf.eprintf "Docktrack: No such update %s in the feature %s\n"
              update_name name;
            ft
      in
      { code_tree with feature_tree = ft' }
  | [ "dock_remove_update"; ft_name; update_name ] ->
      let ft' =
        try
          let ft'' =
            Feature_tree.FeatureTree.remove_update ft_name update_name ft
          in
          Printf.eprintf "Docktrack: Removed update %s\n"
            (Feature_tree.FeatureTree.string_of_update ft_name update_name ft);
          ft''
        with
        | Feature_tree.FeatureTree.MissingFeature name ->
            Printf.eprintf "Docktrack: No such feature %s in the feature tree\n"
              name;
            ft
        | Feature_tree.FeatureTree.MissingUpdate (name, update_name) ->
            Printf.eprintf "Docktrack: No such update %s in the feature %s\n"
              update_name name;
            ft
      in
      { code_tree with feature_tree = ft' }
  | _ ->
      Printf.eprintf "%s\n"
        "Docktrack: Invalid command. Please use a valid docktrack command.";
      code_tree

let parse_cmd cmd =
  if valid_git_subcmd cmd then GitCommand
  else if valid_docktrack_cmd cmd then DocktrackCommand
  else InvalidCommand

let simulate_command_shell cmd code_tree =
  let parsed_output = parse_cmd cmd in
  match parsed_output with
  | GitCommand ->
      let shell_git_cmd = String.concat ~sep:" " ("git" :: cmd) in
      let git_output = Cli_utils.CliUtils.run_unix shell_git_cmd in
      Cli_utils.CliUtils.print_header "Git";
      Printf.eprintf "%s\n" git_output;
      Cli_utils.CliUtils.print_header "Post-Git";
      Code_tree.CodeTree.validate_code_tree code_tree ()
  | DocktrackCommand ->
      Cli_utils.CliUtils.print_header "Docktrack";
      parse_docktrack_cmd cmd code_tree
  | InvalidCommand ->
      Printf.eprintf "%s\n"
        "Invalid command. Please use a valid git or docktrack command.";
      code_tree

let _ =
  let read_code_tree =
    if Os_utils.OSUtils.validate_file_path Os_utils.OSUtils.code_tree_path ()
    then Code_tree.CodeTree.read_code_tree ()
    else
      Code_tree.CodeTree.empty_ct "docktrack"
        { title = "root"; descr = None; url = None }
  in
  let ref_code_tree = ref read_code_tree in
  let handler cmd =
    let old_tree = !ref_code_tree in
    let new_tree = simulate_command_shell cmd old_tree in
    ref_code_tree := new_tree;
    Code_tree.CodeTree.save_code_tree new_tree ()
  in

  (* Input from the CLI args - similar to what the final product would do *)
  Command_unix.run (Cli_utils.CliUtils.read_input_cmd handler)
