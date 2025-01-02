open Feature_tree
(* open Pretty_print *)

let root_m : FeatureTree.m = {
  title="root";
  url=None;
  descr=None;
} and child1 : FeatureTree.m = {
    title="child1";
    url=None;
    descr=None;
} and child2 : FeatureTree.m = {
    title="child2";
    url=None;
    descr=None;
} and child3 : FeatureTree.m = {
    title="child3";
    url=None;
    descr=None;
}

let feature_tree = FeatureTree.init_project "root" root_m
let feature_tree = 
  FeatureTree.(
    feature_tree 
    |> add_feature "child1" "root" child1 
    |> add_feature "child2" "root" child2 
    |> add_feature "child3" "child1" child3
  )

let _ = FeatureTree.print_tree feature_tree

let feature_tree = 
  FeatureTree.(
    feature_tree 
    |> remove_feature "child1"
  )

let _ = FeatureTree.print_tree feature_tree