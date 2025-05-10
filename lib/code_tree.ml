open Ds_utils
open Core

module CodeTree = struct
  type sst = (string, String.comparator_witness) Set.t
  type file = { name : string; path : string; feature_names : sst }
  type smp = (string, file, String.comparator_witness) Map.t
  type ct = { file_map : smp; feature_tree : Feature_tree.FeatureTree.ft }

  exception DuplicateFile of string
  exception MissingFile of string

  let empty_ct project_name project_data =
    {
      file_map = Map.empty (module String);
      feature_tree =
        Feature_tree.FeatureTree.empty_feature_tree project_name project_data;
    }

  let add_file file_name file_data { file_map; feature_tree } =
    let file =
      {
        name = file_name;
        path = file_data.path;
        feature_names = Set.empty (module String);
      }
    in
    let file_map' = Map.add file_map ~key:file_name ~data:file in
    match file_map' with
    | `Ok map -> { file_map = map; feature_tree }
    | `Duplicate -> raise (DuplicateFile file_name)

  let update_file file_name new_file_data { file_map; _ } =
    let a = Map.remove file_map file_name in
    Map.add a ~key:file_name ~data:new_file_data

  let add_feature file_name feature_name { file_map; feature_tree } =
    let file =
      match Map.find file_map file_name with
      | None -> raise (MissingFile file_name)
      | Some file' -> file'
    in
    if Set.mem file.feature_names feature_name then
      raise (Feature_tree.FeatureTree.DuplicateFeatureName feature_name);
    if not (Map.mem feature_tree.feature_map feature_name) then
      raise (Feature_tree.FeatureTree.MissingFeature feature_name)
    else
      let file' =
        { file with feature_names = Set.add file.feature_names feature_name }
      in
      let file_map' = Map.add file_map ~key:file_name ~data:file' in
      match file_map' with
      | `Ok map -> { file_map = map; feature_tree }
      | `Duplicate -> raise (DuplicateFile file_name)

  let remove_feature file_name feature_name { file_map; feature_tree } =
    let file = Map.find file_map file_name in
    let file = Ds_utils.bind_exn file (MissingFile file_name) in
    if not (Set.mem file.feature_names feature_name) then
      raise (Feature_tree.FeatureTree.MissingFeature feature_name)
    else
      let file' =
        { file with feature_names = Set.remove file.feature_names feature_name }
      in
      let file_map' = Map.add file_map ~key:file_name ~data:file' in
      match file_map' with
      | `Ok map -> { file_map = map; feature_tree }
      | `Duplicate -> raise (DuplicateFile file_name)

  let feature_exists { feature_tree; _ } feature_name =
    Map.mem feature_tree.feature_map feature_name

  let print_code_tree { file_map; _ } =
    if Map.is_empty file_map then
      Printf.printf "There are no files associated with any features yet!\n"
    else
      Map.iter file_map ~f:(fun file ->
          let feature_names =
            SetUtils.string_of_string_set ~sep:',' file.feature_names
          in
          Printf.printf "File: %s\nFeatures: %s\n" file.name feature_names)
end
