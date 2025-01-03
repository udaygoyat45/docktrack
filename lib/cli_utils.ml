module CliUtils = struct
  let print_dashed_line = fun () -> Printf.printf "-----------------------------------\n"
  let print_header header = 
    print_dashed_line ();
    Printf.printf "%s\n" header;
    print_dashed_line ()
end 