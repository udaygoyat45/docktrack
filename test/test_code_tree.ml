open Code_tree
open Cli_utils

let _ = CliUtils.print_header "TESTING CODE TREE"
let _ = CliUtils.print_dashed_line

let _ = 
  CodeTree.empty_ct
  |> CodeTree.add_feature "test.ml" "test"
  |> CodeTree.add_feature "test.ml" "test2"
  |> CodeTree.add_feature "test2.ml" "test"
  |> CodeTree.add_feature "test2.ml" "test2"
