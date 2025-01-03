open Feature_tree

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
    feature_tree: FeatureTree.t;
  }

  let empty_ct = {
    file_map = StringMap.empty;
    feature_tree = FeatureTree.empty_feature_tree "code tree";
  }

  let update_file file_name new_file_data {file_map}  = 
    file_map
    |> StringMap.remove file_name
    |> StringMap.add file_name new_file_data
  
  let add_feature file_name feature_name {file_map} = 
    let file = StringMap.find file_name file_map in
    if StringSet.mem feature_name file.feature_names then
      raise (FeatureTree.DuplicateFeatureName feature_name)
    else
      let file' = { file with feature_names = StringSet.add feature_name file.feature_names } in
      let file_map' = file_map |> StringMap.add file_name file' in
      {file_map = file_map'}

  let remove_feature file_name feature_name {file_map} = 
    let file = StringMap.find file_name file_map in
    if not (StringSet.mem feature_name file.feature_names) then
      raise (FeatureTree.MissingFeature feature_name)
    else
      let file' = { file with feature_names = StringSet.remove feature_name file.feature_names } in
      let file_map' =file_map |> StringMap.add file_name file' in
      {file_map = file_map'}
end
