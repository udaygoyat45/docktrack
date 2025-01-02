open Feature_update 
open Pretty_print

(* 
Feature Tree Needs To Support 3 Essential Features
[x] Addition/Deletion of Features
[ ] Chronological Addition of Updates 
[ ] Addition/Deletion of Targetted Files
*)

module FeatureTree = struct
  module StringSet = Set.Make (String)
  module StringMap = Map.Make (String)

  type 'a ft_map = 'a StringMap.t 

  type m = {
    title: string;
    descr: string option;
    url: string option;
  }

  type t = {
    depth: int;
    metadata: m;
    name: string;
    parent: string option;
    children: StringSet.t;
    documented_updates: FeatureUpdateSet.t;
    undocumented_updates: FeatureUpdateSet.t;
  }

  type ft = {
    root_name: string;
    feature_map: t ft_map
  }

  exception MissingParentFeature
  exception DuplicateFeatureName
  exception DeletingProjectRoot
  exception MissingFeature

  let empty_feature_aux name parent_name depth metadata = {
    name=name;
    depth=depth;
    metadata=metadata;
    documented_updates=FeatureUpdateSet.empty;
    undocumented_updates=FeatureUpdateSet.empty;
    parent=parent_name;
    children=StringSet.empty;
  }

  let init_project project_name metadata = 
    let feature = empty_feature_aux project_name None 0 metadata in
    let feature_map = StringMap.(empty |> add project_name feature) in 
    { root_name=project_name; feature_map}

  let sub_features_exist feature = 
    StringSet.cardinal feature.children > 0
    
  let add_sub_feature_aux sub_feature feature = 
    match feature with 
    | Some feature -> 
      let children = StringSet.add sub_feature.name feature.children in
      Some {feature with children=children}
    | None -> None

  let add_feature name parent_name metadata {feature_map; root_name} =
    if StringMap.find_opt name feature_map <> None then
      raise DuplicateFeatureName;
    match StringMap.find_opt parent_name feature_map with
    | None -> raise MissingParentFeature
    | Some parent_feature -> 
      let feature = empty_feature_aux name (Some parent_name) (parent_feature.depth + 1) metadata in
      let feature_map' = feature_map
        |> StringMap.update parent_name (add_sub_feature_aux feature)
        |> StringMap.add name feature in
      {feature_map=feature_map'; root_name}
  

  let get_feature name {feature_map; _}= 
    match StringMap.find_opt name feature_map with
    | None -> raise MissingFeature
    | Some feature -> feature
  
  let rec remove_feature_aux feature_tree feature_name =
    let feature = get_feature feature_name feature_tree in
    let feature_tree' = StringSet.fold
      (fun child_name ft -> remove_feature_aux ft child_name)
      feature.children
      feature_tree in
    {feature_tree' with feature_map = StringMap.remove feature_name feature_tree'.feature_map}

  let remove_child_aux feature_name = function
  | None -> None
  | Some feature -> Some{feature with children = StringSet.remove feature_name feature.children}

  let remove_feature feature_name feature_tree = 
    if feature_tree.root_name = feature_name then
      raise DeletingProjectRoot;
    let feature = get_feature feature_name feature_tree in
    let parent_name = 
      match feature.parent with
      | None -> raise DeletingProjectRoot
      | Some name -> name in
    let feature_map' = StringMap.update parent_name (remove_child_aux feature_name) feature_tree.feature_map in
    remove_feature_aux {feature_map=feature_map'; root_name=feature_tree.root_name} feature_name 
  
  let string_of_feature feature =
    (PrettyPrint.n_block feature.depth) ^ PrettyPrint.branch ^ " " ^ feature.name
  
  let rec print_tree_aux feature_map feature_name = 
    let feature = StringMap.find feature_name feature_map in
    Printf.printf "%s\n" (string_of_feature feature);
    StringSet.iter (print_tree_aux feature_map) feature.children

  let print_tree {root_name; feature_map} = 
    print_tree_aux feature_map root_name
end 