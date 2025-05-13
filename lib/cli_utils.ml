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

  let git_subcmds =
    [
      "init";
      "clone";
      "status";
      "add";
      "commit";
      "push";
      "pull";
      "fetch";
      "branch";
      "checkout";
      "merge";
      "rebase";
      "log";
      "diff";
      "reset";
      "revert";
      "tag";
      "stash";
      "remote";
      "show";
    ]

  let docktrack_subcmds =
    [
      "add-feature";
      "remove-feature";
      "add-file";
      "remove-files";
      "view-code-tree";
      "view-features";
      "view-feature";
      "add-update";
      "view-updates";
      "view-update";
      "document-next-update";
      "document-update";
      "remove-update";
    ]

  let create_basic_command summary handler =
    let open Command.Param in
    let wrapped =
      map
        (anon (sequence ("cmd" %: string)))
        ~f:(fun cmd -> fun () -> handler cmd)
    in
    Command.basic ~summary wrapped

  let create_group cmd summary handler valid_cmds =
    let rec create_git_commands' cmds =
      match cmds with
      | hd :: rest ->
          let rest_cmds = create_git_commands' rest in
          let c_cmd =
            (hd, create_basic_command (cmd ^ " " ^ hd) (handler hd))
          in
          c_cmd :: rest_cmds
      | [] -> []
    in
    let cmd_grp = create_git_commands' valid_cmds in
    Command.group ~summary cmd_grp

  let git_docktrack_combined summary git_handler docktrack_handler =
    let git_group =
      create_group "git"
        "Docktrack has custom commands accesed through git subcmd" git_handler
        git_subcmds
    in
    let docktrack_group =
      create_group "docktrack"
        "Docktrack has custom commands accesed through dock subcmd"
        docktrack_handler docktrack_subcmds
    in
    Command.group ~summary [ ("git", git_group); ("dock", docktrack_group) ]

  let trim_string str =
    let rec trim_string' str in_quote =
      if String.is_empty str then str
      else
        let fl = String.get str 0 in
        let ll = String.get str (String.length str - 1) in
        if Char.equal fl ' ' then
          trim_string'
            (String.sub str ~pos:1 ~len:(String.length str - 1))
            in_quote
        else if Char.equal ll ' ' then
          trim_string'
            (String.sub str ~pos:0 ~len:(String.length str - 1))
            in_quote
        else if
          (not in_quote)
          && String.length str > 2
          && Char.equal fl '"' && Char.equal ll '"'
        then
          trim_string'
            (String.sub str ~pos:1 ~len:(String.length str - 2))
            in_quote
        else str
    in
    trim_string' str false

  type input_validate = { validator : string -> bool; error_msg : string }

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

  let inline_input_multi ?optional query input_validators general_error_msg =
    let requery error_msg () =
      Printf.printf "%s\n" error_msg;
      Printf.printf "%s: " query;
      Out_channel.flush stdout
    in
    let rec loop_validate' input input_validators' =
      match input_validators' with
      | { validator; error_msg } :: rest ->
          if validator input then loop_validate' input rest
          else `Error error_msg
      | [] -> `Ok
    in
    printf "%s: " query;
    Out_channel.flush stdout;
    let rec inline_input' () =
      let output = String.strip (In_channel.input_line_exn In_channel.stdin) in
      if String.length output = 0 then (
        match optional with
        | Some opt_val -> opt_val
        | None ->
            requery general_error_msg ();
            inline_input' ())
      else
        match loop_validate' output input_validators with
        | `Ok -> output
        | `Error error_msg ->
            requery error_msg ();
            inline_input' ()
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
