[@@@warning "-26"]

open Core
open Cli_utils

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
    ]
  in
  match parsed_cmd with
  | [] -> false
  | first :: _ -> List.mem docktrack_subcmds first ~equal:String.equal

let parse_docktrack_cmd cmd (code_tree : Code_tree.CodeTree.ct) =
  let split_cmd = String.split ~on:' ' cmd in
  let ft = code_tree.feature_tree in
  match split_cmd with
  | [ "dock_view_code_tree" ] ->
      Code_tree.CodeTree.print_code_tree code_tree;
      code_tree
  | [ "dock_view_features" ] ->
      Feature_tree.FeatureTree.print_tree ft;
      code_tree
  | [ "dock_view_feature"; ft_name ] ->
      Feature_tree.FeatureTree.print_feature ft_name ft;
      code_tree
  | "dock_add_file" :: _ ->
      let n_file_path =
        CliUtils.inline_input "File Path"
          (fun path -> Os_utils.OSUtils.validate_file_path path ())
          "Please provide an accurate path from the root of the project dir"
      in
      let rec add_features features () =
        match CliUtils.inline_input_bool "Add feature to file?" with
        | Yes ->
            let n_ft_name =
              CliUtils.inline_input "Feature name"
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
      code_tree
  | "dock_remove_files" :: _ -> code_tree
  | "dock_add_feature" :: _ ->
      let n_ft =
        CliUtils.inline_input "Feature"
          (fun x -> String.length x > 0)
          "Feature name cannot be empty"
      in
      let n_pft =
        CliUtils.inline_input ~optional:ft.root_name "Parent (optional)"
          (fun _ -> true)
          "Parent feature name cannot be empty"
      in
      let n_title =
        CliUtils.inline_input "Feature title"
          (fun x -> String.length x > 0)
          "Feature title cannot be empty"
      in
      let n_descr =
        CliUtils.inline_input ~optional:"" "Feature description (optional)"
          (fun x -> String.length x > 10)
          "Feature description should be more than 10 characters"
      in
      let n_url =
        CliUtils.inline_input ~optional:"" "Feature URL"
          (fun _ -> true)
          "Feature URL cannot be empty"
      in
      CliUtils.print_header "Feature Metadata";
      printf "feature: %s, parent: %s, metadata: %s; %s; %s\n" n_ft n_pft
        n_title n_descr n_url;
      let n_metadata : Feature_tree.FeatureTree.m =
        {
          title = n_title;
          descr = (if String.length n_descr = 0 then Some n_descr else None);
          url = (if String.length n_url = 0 then Some n_url else None);
        }
      in
      let ft' = Feature_tree.FeatureTree.add_feature n_ft n_pft n_metadata ft in
      { code_tree with feature_tree = ft' }
  | "dock_remove_feature" :: ft_name ->
      let ft_name' = String.concat ~sep:" " ft_name in
      let ft' = Feature_tree.FeatureTree.remove_feature ft_name' ft in
      { code_tree with feature_tree = ft' }
  | _ ->
      print_endline
        "Docktrack: Invalid command. Please use a valid docktrack command.";
      code_tree

let parse_cmd cmd =
  let split_cmd = String.split ~on:' ' cmd in
  if valid_git_subcmd split_cmd then GitCommand
  else if valid_docktrack_cmd split_cmd then DocktrackCommand
  else InvalidCommand

let simulate_command_shell cmd code_tree =
  let parsed_output = parse_cmd cmd in
  match parsed_output with
  | GitCommand ->
      let shell_git_cmd = "git " ^ cmd in
      let git_output = CliUtils.run_unix shell_git_cmd in
      CliUtils.print_header "Git";
      print_endline git_output;
      code_tree
  | DocktrackCommand ->
      CliUtils.print_header "Docktrack";
      parse_docktrack_cmd cmd code_tree
  | InvalidCommand ->
      print_endline
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
  let handler words =
    let full_cmd = String.concat ~sep:" " words in
    let old_tree = !ref_code_tree in
    let new_tree = simulate_command_shell full_cmd old_tree in
    ref_code_tree := new_tree;
    Code_tree.CodeTree.save_code_tree new_tree ()
  in

  (* handler [ "dock_add_feature" ];
  handler [ "dock_add_feature" ];
  handler [ "dock_add_file" ]; *)

  (* Feature_tree.FeatureTree.save_feature_tree !ref_code_tree.feature_tree () *)

  (* Input from the CLI args - similar to what the final product would do *)
  Command_unix.run (CliUtils.read_input_cmd handler)

(* handler ["dock_add_feature"];
  handler ["dock_view_features"];
  handler ["dock_view_code_tree"]; *)

(* handler ["dock_view_feature"; "something"]; *)
(* handler ["dock_remove_feature"; "something"];
  handler ["dock_view_features"];
  handler ["dock_view_feature"; "something"] *)

(* let ans : CliUtils.input_bool = CliUtils.inline_input_bool ("Is this testing working?") in 
  match ans  with
  | Yes -> print_endline "Yes recorded"
  | No -> print_endline "No recorded" *)
