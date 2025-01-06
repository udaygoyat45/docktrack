open Feature_tree
open Ds_utils

module CodeTree = struct
  module StringSet = Set.Make(String)
  module StringMap = Map.Make(String)

  type file = {
    name: string;
    path: string;
    feature_names: StringSet.t;
  }

  type ct = {
    file_map: file StringMap.t;
    feature_tree: FeatureTree.ft;
  }

  let empty_ct project_name project_data = {
    file_map = StringMap.empty;
    feature_tree = FeatureTree.empty_feature_tree project_name project_data;
  }


  let add_file file_name file_data {file_map; feature_tree} = 
    let file = {
      name = file_name;
      path = file_data.path;
      feature_names = StringSet.empty;
    } in
    let file_map' = file_map |> StringMap.add file_name file in
    {file_map = file_map'; feature_tree}

  let update_file file_name new_file_data {file_map; _}  = 
    file_map
    |> StringMap.remove file_name
    |> StringMap.add file_name new_file_data
  
  let add_feature file_name feature_name {file_map; feature_tree} = 
    let file = StringMap.find file_name file_map in
    if StringSet.mem feature_name file.feature_names then
      raise (FeatureTree.DuplicateFeatureName feature_name);
    if not (StringMap.mem feature_name feature_tree.feature_map) then
      raise (FeatureTree.MissingFeature feature_name)
    else
      let file' = { file with feature_names = StringSet.add feature_name file.feature_names } in
      let file_map' = file_map |> StringMap.add file_name file' in
      {file_map = file_map'; feature_tree}
  

  let remove_feature file_name feature_name {file_map; feature_tree} = 
    let file = StringMap.find file_name file_map in
    if not (StringSet.mem feature_name file.feature_names) then
      raise (FeatureTree.MissingFeature feature_name)
    else
      let file' = { file with feature_names = StringSet.remove feature_name file.feature_names } in
      let file_map' =file_map |> StringMap.add file_name file' in
      {file_map = file_map'; feature_tree}
  
  let print_code_tree {file_map; _} = 
    StringMap.iter (fun file_name _ -> 
      let file = StringMap.find file_name file_map in
      let feature_names = SetUtils.string_of_string_set ~sep:',' file.feature_names in
      Printf.printf "File: %s\nFeatures: %s\n" file_name feature_names
    ) file_map
end
