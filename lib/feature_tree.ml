open Core
open Sexplib.Std
open Pretty_print

module FeatureTree = struct
  type sst = Set.M(String).t [@@deriving sexp]

  type m = { title : string; descr : string option; url : string option }
  [@@deriving sexp]

  type t = {
    depth : int;
    metadata : m;
    name : string;
    parent : string option;
    children : sst;
    documented_updates : Feature_update.FeatureUpdate.t list;
    undocumented_updates : Feature_update.FeatureUpdate.t list;
  }
  [@@deriving sexp]

  type string_map_ft = t Map.M(String).t [@@deriving sexp]

  type ft = { root_name : string; feature_map : string_map_ft }
  [@@deriving sexp]

  exception MissingParentFeature of string
  exception DuplicateFeatureName of string
  exception DeletingProjectRoot of string
  exception MissingFeature of string
  exception EmptyUndocumentedUpdates

  let empty_feature_aux name parent_name depth metadata =
    {
      name;
      depth;
      metadata;
      documented_updates = [];
      undocumented_updates = [];
      parent = parent_name;
      children = Set.empty (module String);
    }

  let empty_feature_tree project_name metadata =
    let feature = empty_feature_aux project_name None 0 metadata in
    let empty_map = Map.empty (module String) in
    let feature_map = Map.add_exn empty_map ~key:project_name ~data:feature in
    { root_name = project_name; feature_map }

  let sub_features_exist feature = Set.length feature.children > 0

  let add_sub_feature_aux sub_feature feature =
    match feature with
    | Some feature ->
        let children = Set.add feature.children sub_feature.name in
        { feature with children }
    | None ->
        raise
          (MissingParentFeature "add_sub_feature_aux called with None feature")

  let add_feature name parent_name metadata { feature_map; root_name } =
    match Map.find feature_map parent_name with
    | None -> raise (MissingParentFeature parent_name)
    | Some parent_feature -> (
        let feature =
          empty_feature_aux name (Some parent_name) (parent_feature.depth + 1)
            metadata
        in
        let feature_map' =
          Map.update feature_map parent_name ~f:(add_sub_feature_aux feature)
        in
        let feature_map'' = Map.add feature_map' ~key:name ~data:feature in
        match feature_map'' with
        | `Duplicate -> raise (DuplicateFeatureName name)
        | `Ok feature_map''' -> { feature_map = feature_map'''; root_name })

  let get_feature name { feature_map; _ } =
    match Map.find feature_map name with
    | None -> raise (MissingFeature name)
    | Some feature -> feature

  let rec remove_feature_aux feature_tree feature_name =
    let feature = get_feature feature_name feature_tree in
    let feature_tree' =
      Set.fold feature.children ~init:feature_tree ~f:(fun c_ft child ->
          remove_feature_aux c_ft child)
    in
    {
      feature_tree' with
      feature_map = Map.remove feature_tree'.feature_map feature_name;
    }

  let remove_child_aux feature_name = function
    | None -> raise (MissingFeature feature_name)
    | Some feature ->
        { feature with children = Set.remove feature.children feature_name }

  let remove_feature feature_name feature_tree =
    if String.equal feature_tree.root_name feature_name then
      raise (DeletingProjectRoot feature_name);
    let feature = get_feature feature_name feature_tree in
    let parent_name =
      match feature.parent with
      | None -> raise (DeletingProjectRoot feature_name)
      | Some name -> name
    in
    let feature_map' =
      Map.update feature_tree.feature_map parent_name
        ~f:(remove_child_aux feature_name)
    in
    remove_feature_aux
      { feature_map = feature_map'; root_name = feature_tree.root_name }
      feature_name

  (* Code for manipulating updates to a feature *)
  let add_update (update : Feature_update.FeatureUpdate.t) feature_name
      feature_tree =
    let feature = get_feature feature_name feature_tree in
    let feature' =
      match update.status with
      | Feature_update.FeatureUpdate.Documented ->
          {
            feature with
            documented_updates = update :: feature.documented_updates;
          }
      | Feature_update.FeatureUpdate.Undocumented ->
          {
            feature with
            undocumented_updates = update :: feature.undocumented_updates;
          }
    in
    let feature_map' =
      Map.update feature_tree.feature_map feature_name ~f:(fun _ -> feature')
    in
    { feature_map = feature_map'; root_name = feature_tree.root_name }

  let oldest_undocumented_update feature_name feature_tree =
    let feature = get_feature feature_name feature_tree in
    List.hd feature.undocumented_updates

  let newest_documented_update feature_name feature_tree =
    let feature = get_feature feature_name feature_tree in
    List.hd feature.documented_updates

  let document_next_update feature_name feature_tree =
    let feature = get_feature feature_name feature_tree in
    match feature.undocumented_updates with
    | [] -> raise EmptyUndocumentedUpdates
    | update :: undocumented_updates' ->
        let documented_updates' = update :: feature.documented_updates in
        let feature' =
          {
            feature with
            undocumented_updates = undocumented_updates';
            documented_updates = documented_updates';
          }
        in
        let feature_map' =
          Map.update feature_tree.feature_map feature_name ~f:(fun _ ->
              feature')
        in
        { feature_map = feature_map'; root_name = feature_tree.root_name }

  let string_of_feature feature =
    if feature.depth = 0 then feature.name
    else
      PrettyPrint.n_block (feature.depth - 1)
      ^ PrettyPrint.branch ^ " " ^ feature.name

  let rec print_tree_aux feature_map feature_name =
    let feature_opt = Map.find feature_map feature_name in
    match feature_opt with
    | None -> raise (MissingFeature feature_name)
    | Some feature ->
        Printf.printf "%s\n" (string_of_feature feature);
        Set.iter feature.children ~f:(print_tree_aux feature_map)

  let print_tree { root_name; feature_map } =
    print_tree_aux feature_map root_name

  let print_updates feature_name feature_tree =
    let feature = get_feature feature_name feature_tree in
    Printf.printf "Documented updates (latest to oldest):\n";
    if List.length feature.documented_updates > 0 then
      print_string
        (Feature_update.FeatureUpdate.string_of_updates
           feature.documented_updates)
    else Printf.printf "** No documented updates **\n";
    Printf.printf "Undocumented updates (latest to oldest):\n";
    if List.length feature.undocumented_updates > 0 then
      print_string
        (Feature_update.FeatureUpdate.string_of_updates
           feature.undocumented_updates)
    else Printf.printf "** No undocumented updates **\n"

  let print_feature feature_name feature_tree =
    let feature = get_feature feature_name feature_tree in
    Printf.printf "Feature: %s\n" feature.name;
    Printf.printf "Depth: %d\n" feature.depth;
    Printf.printf "Parent: %s\n"
      (match feature.parent with None -> "(Top Node)" | Some name -> name);
    Printf.printf "Children: %s\n"
      (Set.fold feature.children ~init:"" ~f:(fun child acc ->
           acc ^ child ^ ", "));
    print_updates feature_name feature_tree

  let save_feature_tree feature_tree () =
    Os_utils.OSUtils.create_dir ".docktrack" ();
    let write_path = Filename.concat ".docktrack" "feature_tree.sexp" in
    let sexp = sexp_of_ft feature_tree in
    Out_channel.with_file write_path ~f:(fun oc ->
        Sexp.to_string_hum sexp |> Out_channel.output_string oc)

  let read_feature_tree () =
    Os_utils.OSUtils.create_dir ".docktrack" ();
    let read_path = Filename.concat ".docktrack" "feature_tree.sexp" in
    let data = In_channel.read_all read_path in
    let sexp = Sexp.of_string data in
    ft_of_sexp sexp
end
