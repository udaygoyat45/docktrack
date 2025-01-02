open Feature_update 
open Ds_utils

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
(* 
module type FeatureTree = sig
  type t
  type m
  type ft

  exception MissingParentFeature
  exception DuplicateFeatureName

  val new_feature : m -> t
  val add_feature : m -> string -> string StringMap.t -> string StringMap.t
end *)

module FeatureTree = struct
  type 'a ft_map = 'a StringMap.t 

  type m = {
    title: string;
    descr: string option;
    url: string option;
  }

  type t = {
    name: string;
    depth: int;
    metadata: m;
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
  exception MissingFeature


  let empty_feature_aux name depth metadata = {
    name=name;
    depth=depth;
    metadata=metadata;
    documented_updates=FeatureUpdateSet.empty;
    undocumented_updates=FeatureUpdateSet.empty;
    children=StringSet.empty;
  }


  let init_project project_name metadata = 
    let feature = empty_feature_aux project_name 0 metadata in
    let feature_map = StringMap.(empty |> add project_name feature) in 
    { root_name=project_name; feature_map}

    
  let add_sub_feature_aux sub_feature feature = 
    match feature with 
    | Some feature -> 
      Printf.printf "Debugging: %s %s\n" feature.name sub_feature.name;
      let children = StringSet.add sub_feature.name feature.children in
      SetUtils.print_string_set children;
      Some {feature with children=children}
    | None -> None


  let add_feature name parent_name metadata {feature_map; root_name} =
    if StringMap.find_opt name feature_map <> None then
      raise DuplicateFeatureName;
    match StringMap.find_opt parent_name feature_map with
    | None -> raise MissingParentFeature
    | Some parent_feature -> 
      let feature = empty_feature_aux name (parent_feature.depth + 1) metadata in
      let feature_map' = feature_map
        |> StringMap.update parent_name (add_sub_feature_aux feature)
        |> StringMap.add name feature in
      {feature_map=feature_map'; root_name}

  
  let indent = 5
  let block = "│" ^ (String.make indent ' ' )
  let n_block n = String.concat "" (List.init n (fun _ -> block))
  let string_of_feature feature =
    (n_block feature.depth) ^ "├── " ^ feature.name
  
  let rec print_tree_aux feature_map feature_name = 
    let feature = StringMap.find feature_name feature_map in
    Printf.printf "%s\n" (string_of_feature feature);
    StringSet.iter (print_tree_aux feature_map) feature.children

  let print_tree {root_name; feature_map} = 
    print_tree_aux feature_map root_name
end 