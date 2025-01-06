open Code_tree
open Feature_tree
open Cli_utils

let _ = CliUtils.print_header "TESTING CODE TREE"
let _ = CliUtils.print_dashed_line

module StringSet = Set.Make(String)

let project_name = "auto documentation"
let project_data : FeatureTree.m = {
    title="auto documentation title";
    url=None;
    descr=None; 
}

let code_tree = CodeTree.empty_ct project_name project_data

(* Steps for adding a new feature which doesn't exiset yet:
  1. Add teh feature to the feature tree (FeatureTree.add_feature)
  2. Add the feature to the code tree (CodeTree.add_feature)
*)
let new_feature_to_ct name parent_name metadata (code_tree : CodeTree.ct) = 
  let feature_tree' = FeatureTree.add_feature name parent_name metadata code_tree.feature_tree in
  {code_tree with feature_tree = feature_tree'}


let child1 : FeatureTree.m = {
    title="feature tree title";
    url=None;
    descr=None;
} and child2 : FeatureTree.m = {
    title="feature update title";
    url=None;
    descr=None;
} and child3 : FeatureTree.m = {
    title="pretty print title";
    url=None;
    descr=None;
} and child4 : FeatureTree.m = {
    title="ds utils title";
    url=None;
    descr=None;
}

let code_tree = code_tree
  |> new_feature_to_ct "feature tree" "auto documentation" child1
  |> new_feature_to_ct "feature update" "auto documentation" child2
  |> new_feature_to_ct "pretty print" "auto documentation" child3
  |> new_feature_to_ct "ds utils" "auto documentation" child4

  |> CodeTree.add_file "test.ml" ({
    name = "test.ml"; path = "test.ml"; feature_names = StringSet.empty
    } : CodeTree.file)

  |> CodeTree.add_feature "test.ml" "feature tree"
  |> CodeTree.add_feature "test.ml" "feature update"
  |> CodeTree.add_feature "test.ml" "pretty print"
  |> CodeTree.add_feature "test.ml" "ds utils"

let _ = CodeTree.print_code_tree code_tree