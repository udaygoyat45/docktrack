open Core

module CliUtils = struct
  type input_bool = Yes | No
  let print_dashed_line =
   fun () -> Printf.printf "-----------------------------------\n"

  let print_header header =
    print_dashed_line ();
    Printf.printf "%s\n" header;
    print_dashed_line ()

  let run_unix cmd =
    let inp = Core_unix.open_process_in cmd in
    let r = In_channel.input_all inp in
    In_channel.close inp;
    r

  let read_input_cmd parse_input_cmd =
    let open Command.Param in
    let wrapped =
      map
        (anon (sequence ("cmd" %: string)))
        ~f:(fun cmd -> fun () -> parse_input_cmd cmd)
    in
    Command.basic ~summary:"Wrapping git commands with docktrack"
      ~readme:(fun () -> "This command wraps git commands with docktrack.")
      wrapped


  let trim_string str = 
    let rec trim_string' str in_quote = 
      if String.is_empty str then str
      else (
        let fl = String.get str 0 in
        let ll = String.get str (String.length str - 1) in
        if Char.equal fl ' ' then
          trim_string' (String.sub str ~pos:1 ~len:(String.length str - 1)) in_quote
        else if Char.equal ll ' ' then
          trim_string' (String.sub str ~pos:0 ~len:(String.length str - 1)) in_quote
        else if not in_quote && String.length str > 2 && Char.equal fl '"' && Char.equal ll '"' then
          trim_string' (String.sub str ~pos:1 ~len:(String.length str - 2)) in_quote
        else
          str
      )
        in trim_string' str false


  let inline_input ?optional query validator error_msg =
    let requery () =
      Printf.printf "%s\n" error_msg;
      Printf.printf "%s: " query;
      Out_channel.flush stdout
    in
    printf "%s: " query;
    Out_channel.flush stdout;
    let rec inline_input' () =
      let output = String.strip (In_channel.input_line_exn In_channel.stdin) in
      if String.length output = 0 then (
        match optional with
        | Some opt_val -> opt_val
        | None ->
            requery ();
            inline_input' ())
      else if validator output then output
      else (
        requery ();
        inline_input' ())
    in
    let raw_input = inline_input' () in
    trim_string raw_input


  let inline_input_bool query =
    let valid_yes =
      Set.of_list (module String) [ "Y"; "y"; "yes"; "YES"; "" ]
    in
    let valid_no = Set.of_list (module String) [ "N"; "n"; "no"; "NO" ] in
    let rec loop () =
      Printf.printf "%s [Y/n]: " query;
      Out_channel.flush stdout;
      let output = In_channel.input_line_exn In_channel.stdin |> String.strip in
      if Set.mem valid_yes output then Yes
      else if Set.mem valid_no output then No
      else loop ()
    in
    loop ()
end
