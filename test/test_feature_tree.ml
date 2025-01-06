open Feature_tree
open Feature_update
open Cli_utils

let root_m : FeatureTree.m = {
  title="auto documentation title";
  url=None;
  descr=None;
} and child1 : FeatureTree.m = {
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

let update1 : FeatureUpdate.t = {
  title="addition";
  content="content1";
  timestamp=1;
  status=FeatureUpdate.Documented;
} and  update1_2 : FeatureUpdate.t = {
  title="deletion";
  content="content2";
  timestamp=1;
  status=FeatureUpdate.Documented;
} and update2 : FeatureUpdate.t = {
  title="chronological update";
  content="content1";
  timestamp=2;
  status=FeatureUpdate.Undocumented;
}

let _ = CliUtils.print_header "TESTING FEATURE TREE"
let feature_tree = FeatureTree.empty_feature_tree "auto documentation" root_m
let feature_tree = 
  FeatureTree.(
    feature_tree 
    |> add_feature "feature tree" "auto documentation" child1 
    |> add_feature "feature update" "auto documentation" child2 
    |> add_feature "pretty print" "auto documentation" child3
    |> add_feature "ds utils" "auto documentation" child4
  )

let _ = FeatureTree.print_tree feature_tree; 
CliUtils.print_dashed_line

let feature_tree = 
  FeatureTree.(
    feature_tree 
    |> remove_feature "ds utils"
  )

let _ = FeatureTree.print_tree feature_tree;
CliUtils.print_dashed_line

let feature_tree = feature_tree 
  |> FeatureTree.add_update update1 "feature tree"
  |> FeatureTree.add_update update1_2 "feature tree"
  |> FeatureTree.add_update update2 "feature tree"

let feature_tree = feature_tree 
  |> FeatureTree.document_next_update "feature tree"


let _ = FeatureTree.print_updates "feature tree" feature_tree; 
CliUtils.print_dashed_line